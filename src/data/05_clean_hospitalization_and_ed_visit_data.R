# Clean parsed and annotated hospitalization and ED visit data and combine same- and/or
# next-day encounters into single events

#### set-up ----

library(tidyverse)
library(lubridate)


#### read in data ----

# `data/interim/unplanned_healthcare_event_dates.csv` (output of `01_parse_hospitalization_and_ed_visits.R` script)
# was reviewed and manually annotated by author LK; this is the file we read in here
healthcare_events <- readxl::read_excel(
    here::here("data/interim/unplanned_healthcare_event_dates_LK updates_120323_final_updated_again_121823.xlsx"),
    col_types = c("numeric", "date", "text", "text", "text", "date", "date", "numeric", "text", "text")
  ) 


#### drop ineligible encounters ----

healthcare_events <- healthcare_events %>% 
  mutate(
    visit_reason = ifelse(visit_reason == "NA", NA_character_, visit_reason),
    visit_notes = ifelse(visit_notes == "NA", NA_character_, visit_notes),
    # include_yes_no column created manually during review and contains decision and notes about decision for each visit
    include = case_when(
      grepl("^yes", include_yes_no) ~ 1,
      grepl("^no", include_yes_no) ~ 0,
      TRUE ~ NA_real_
    )
  ) 

dropped_healthcare_events <- healthcare_events %>%
  filter(include == 0)

included_healthcare_events <- healthcare_events %>%
  filter(include == 1)


#### combine same- and next-day encounters ----

# patients may: (1) visit the ED more than one time in a given day,  (2) present at the ED and be hospitalized on the same day, or 
# (3) present at the ED and be hospitalized the next day (e.g., if they presented to the ED late in the evening and the hospital 
# admission occurred after midnight).  Rather than treat these events separately, we consider them as representing a single event    
combined_included_healthcare_events <- included_healthcare_events %>%
  arrange(record_id, date, visit_type) %>%
  group_by(record_id) %>%
  mutate(
    days_since_previous_visit = as.numeric(date - lag(date), units = "days"),
    previous_visit_type = lag(visit_type),
    # data must be arranged by visit type above for this logic to apply
    visit_category = case_when(
      visit_type == "Hospitalization" & previous_visit_type == "ED" & days_since_previous_visit == 0 ~ "ED visit and same-day hospitalization",
      visit_type == "ED" & lead(visit_type) == "Hospitalization" & lead(days_since_previous_visit) == 0 ~ "ED visit and same-day hospitalization",
      visit_type == "Hospitalization" & previous_visit_type == "ED" & days_since_previous_visit == 1 ~ "ED visit and next-day hospitalization",
      visit_type == "ED" & lead(visit_type) == "Hospitalization" & lead(days_since_previous_visit) == 1 ~ "ED visit and next-day hospitalization",
      visit_type == "ED" & previous_visit_type == "ED" &  days_since_previous_visit == 0 ~ "Multiple same-day ED visits",
      visit_type == "ED" & lead(previous_visit_type) == "ED" & lead(days_since_previous_visit) == 0 ~ "Multiple same-day ED visits",
      TRUE ~ ifelse(visit_type == "ED", "ED visit only", "Hospitalization only")
    )
  ) %>%
  ungroup() %>%
  group_by(record_id, date, visit_category) %>%
  summarize(
    visit_reason = paste0(visit_reason, collapse = "/"),
    # these values are missing if the event did not involve hospitalization 
    hospitalization_admit_date = as.Date(ifelse(!all(is.na(hospitalization_admit_date)), as.character(min(hospitalization_admit_date, na.rm = TRUE)), NA_character_)),
    hospitalization_discharge_date = as.Date(ifelse(!all(is.na(hospitalization_discharge_date)), as.character(max(hospitalization_discharge_date, na.rm = TRUE)), NA_character_)),
    hospitalization_length_of_stay_days = ifelse(!all(is.na(hospitalization_length_of_stay_days)), max(hospitalization_length_of_stay_days, na.rm = TRUE), NA)
  ) %>%
  ungroup()


#### save cleaned event data ----

write_csv(dropped_healthcare_events, here::here("data/interim/dropped_unplanned_healthcare_events.csv"))
write_csv(included_healthcare_events, here::here("data/interim/included_unplanned_healthcare_events.csv"))
write_csv(combined_included_healthcare_events, here::here("data/processed/rosa_combined_unplanned_healthcare_events.csv")) 
