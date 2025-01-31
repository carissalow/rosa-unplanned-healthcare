# Table 2: Descriptives of unplanned healthcare events

#### set up ----

library(tidyverse)
library(gtsummary)
library(gt)

source(here::here("src/tables/utils.R"))
source(here::here("src/analysis/01_format_individual_symptoms_data_for_analysis.R"))


#### format data ----

healthcare_events_for_table <- healthcare_events %>%
  select(record_id, date, study_day, starts_with("visit"), starts_with("hospitalization")) %>%
  filter(!is.na(visit_category)) %>%
  mutate(visit_year = lubridate::year(date))

encounters_per_participant <- healthcare_events %>%
  filter(!is.na(visit_category)) %>%
  summarize(.by = "record_id", number_of_encounters = n()) 


#### create table 2 ----

table_2 <- healthcare_events_for_table %>%
  select(record_id, visit_category, visit_year, study_day, hospitalization_length_of_stay_days) %>%
  tbl_summary(
    include = -c("record_id"),
    label = list(
      visit_category = "Encounter type",
      visit_year = "Time of encounter, year",
      study_day = "Time of encounter, days since enrollment",
      hospitalization_length_of_stay_days = "Hospitalization length of stay, days"
    ),
    type = list(
      all_dichotomous() ~ "categorical"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) [{min}, {max}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(2, 2, 0, 0),
      all_categorical() ~ c(0, 1)
    ),
    missing = "no"
  ) %>%
  bold_labels() %>%
  as_gt() %>%
  gt::opt_align_table_header(
    align = "left"
  ) %>%
  gt::tab_header(
    md("**Table 2. Event characteristics**")
  ) 

gt::gtsave(table_2, here::here("output/tables/table_2.docx"))
