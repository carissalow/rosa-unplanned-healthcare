# Assign binary labels reflecting occurrence of one or more unplanned healthcare events
# of any type (ED only, hospitalization only, or ED & hospitalization) within 7 days to 
# each study day for each participant

#### set-up ----

library(tidyverse)


#### read in data ----

healthcare_events <- read_csv(here::here("data/processed/rosa_combined_unplanned_healthcare_events.csv"))
study_days <- read_csv(here::here("data/processed/rosa_study_dates_and_indexes.csv"))


#### create binary labels for occurrence of different event types in case we wish to examine separately ----

# create binary labels for occurrence of unplanned healthcare events overall and by category
healthcare_events <- healthcare_events %>%
  mutate(
    date = as.Date(date),
    unplanned_healthcare_event = 1,
    ed_only = ifelse(visit_category == "ED visit only", 1, 0),
    hospital_only = ifelse(visit_category == "Hospitalization only", 1, 0),
    ed_and_hospital = ifelse(visit_category == "ED visit and same-day hospitalization", 1, 0)
  )


#### create binary labels reflecting occurrence of at least one event of any type within 7 days of the present day (including that day) ----

healthcare_events_within_7day_windows <- study_days %>%
  select(-c(study_completion_status, days_in_study)) %>%
  left_join(healthcare_events, by = c("record_id", "date")) %>%
  mutate(across(c("unplanned_healthcare_event", "ed_only", "hospital_only", "ed_and_hospital"), function(x) ifelse(is.na(x), 0, x))) %>%
  group_by(record_id) %>%
  mutate(
    # 7-day window includes/begins on the current day (vs. beginning the following day)
    # to directly get the window to being on the following day, the logic is: [date <= (.x + lubridate::days(7)) & date > .x]
    unplanned_healthcare_event_within_7days_incl_today = purrr::map_dbl(date, ~sum(unplanned_healthcare_event[date < (.x + lubridate::days(7)) & date >= .x])),
    unplanned_healthcare_event_within_7days_incl_today  = ifelse(unplanned_healthcare_event_within_7days_incl_today >= 1, 1, 0),
  ) %>%
  # we re-assign NA values to labels of 0 if the 7-day window is truncated by the end of the study observation period.
  # e.g., if no events occurred within 2 days of the end of the study, this would initially receive a label of 0
  # however, we do not have information about the following 5 days -- an event could have occurred during that time, so
  # we cannot be certain that a label of 0 truly corresponds to the lack of unplanned healthcare events in the next 7 days 
  # in that case. To be conservative, we treat this as unknown. However, if an event did occur within 2 days of the end of 
  # the study, this initially receives a label of 1 and this does not change
  mutate(max_date = as.Date(ifelse(is.na(as.numeric(lead(date)-date)) | as.numeric(lead(date)-date) != 1, date, NA_Date_))) %>%
  fill(max_date, .direction = "up") %>%
  mutate(days_between_today_and_max_date = as.numeric(max_date - date) + 1) %>%
  mutate(
    unplanned_healthcare_event_within_7days_incl_today_with_unknowns = case_when(
      unplanned_healthcare_event_within_7days_incl_today == 0 & days_between_today_and_max_date < 7 ~ NA,
      TRUE ~ unplanned_healthcare_event_within_7days_incl_today
    )
  ) %>%
  # finally, we shift this variable by 1 day such that the 7-day window begins on the following day (i.e., reflecting events within the *following* 7 days)
  mutate(
    unplanned_healthcare_event_within_7days_excl_today_with_unknowns = lead(unplanned_healthcare_event_within_7days_incl_today_with_unknowns)
  ) %>%
  ungroup() %>%
  select(-c(max_date, days_between_today_and_max_date))


#### save results ----

write_csv(healthcare_events_within_7day_windows, here::here("data/processed/rosa_combined_unplanned_healthcare_events_within_7day_windows.csv"))
