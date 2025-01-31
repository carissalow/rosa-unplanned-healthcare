# Format data for individual symptom analysis:
# - Merge outcomes, predictors, and covariates
# - Convert categorical predictors to factors with meaningful reference level
# - Restrict observations to those with complete outcome, predictor, and covariate data 
#   (i.e., complete cases)


#### set up ----

library(tidyverse)


#### settings ----

exclude_present_day_from_event_window <- FALSE
restrict_to_complete_cases <- TRUE

if (exclude_present_day_from_event_window) {
  outcome_columns <- c("unplanned_healthcare_event_within_7days_excl_today_with_unknowns")
} else {
  outcome_columns <- c("unplanned_healthcare_event_within_7days_incl_today_with_unknowns")
}

join_columns <- c("record_id", "date", "study_day")


#### import data ----

healthcare_events <- read_csv(here::here("data/processed/rosa_combined_unplanned_healthcare_events_within_7day_windows.csv"))
daily_symptom_labels <- read_csv(here::here("data/processed/rosa_daily_symptom_labels_individual.csv"))


#### create dataset for analysis ----

data_for_analysis <- healthcare_events %>%
  select(c(all_of(join_columns), all_of(outcome_columns))) %>%
  left_join(daily_symptom_labels, by = join_columns) %>%
  mutate(
    across(
      ends_with("_symptom_day"),
      function(x) case_when(x == 0 ~ "No", x == 1 ~ "Yes", TRUE ~ NA_character_)
    ),
    across(
      ends_with("_symptom_day"),
      function(x) factor(x, levels = c("No", "Yes"))
    )
  ) 

# drop observations with any missing data
if (restrict_to_complete_cases) {
  data_for_analysis <- data_for_analysis[complete.cases(data_for_analysis), ]
}
