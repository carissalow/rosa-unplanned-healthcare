# Clean inpatient hospitalization and ED visit dates extracted from patient EMR and entered into REDCap
# Restrict to participants who completed the study on or before October 1, 2023

#### set up ----

library(tidyverse)
source(here::here("src/data/utils.R"))

emr_data_file <- "data/raw/ROSA-EMR_DATA_2023-10-25_1651.csv"
excluded_column_patterns <- c("repeat", "cancer", "comorbid", "emr", "deceased", "death", "ecog", "chemo")

min_pid <- 1001
max_pid <- 1183 


#### read in data ----

study_days <- read_csv(here::here("data/processed/rosa_study_dates_and_indexes.csv")) %>%
  select(record_id, date)

emr_data <- read_file_with_clean_names(here::here(emr_data_file), excluded_column_patterns = excluded_column_patterns) %>% 
  subset_participants(participant_id_column = "record_id", participants = seq(min_pid, max_pid, by = 1)) 
  

#### parse hospitalization dates ----

hospitalizations <- select(emr_data, c("record_id", starts_with("hosp")))

hospitalizations <- hospitalizations %>% 
  pivot_longer(
    cols = starts_with("hosp"),
    names_to = c("hospitalization_number", ".value"),
    names_pattern = "hosp(.)_(.*)",
    values_drop_na = TRUE
  ) %>%
  group_by(record_id) %>%
  arrange(admit_date) %>%
  mutate(hospitalization_number = row_number()) %>%
  ungroup() %>%
  mutate(
    date = admit_date,
    visit_type = "Hospitalization"
  ) %>%
  arrange(record_id)


#### parse ED visit dates ----

ed_visits <- select(emr_data, c("record_id", starts_with("edvisit")))

ed_visits <- ed_visits %>%
  pivot_longer(
    cols = starts_with("edvisit"),
    names_to = c("ed_visit_number", ".value"),
    names_pattern = "edvisit(.)_(.*)",
    values_drop_na = TRUE
  ) %>%
  group_by(record_id) %>%
  arrange(date) %>%
  mutate(ed_visit_number = row_number()) %>%
  ungroup() %>%
  mutate(
    visit_type = "ED"
  ) %>%
  arrange(record_id)


#### combine event data ----

unplanned_healthcare_events <- hospitalizations %>%
  bind_rows(ed_visits) %>%
  group_by(record_id) %>%
  arrange(date) %>%
  mutate(visit_number = row_number()) %>%
  ungroup() %>%
  arrange(record_id, date) %>%
  select(record_id, date, visit_number, visit_type, reason, notes, admit_date, discharge_date, hospitalization_number, ed_visit_number) %>%
  # drop visits that occurred outside of study participation window
  inner_join(study_days, by = c("record_id", "date"))

write_csv(unplanned_healthcare_events, here::here("data/interim/unplanned_healthcare_event_dates.csv"))
