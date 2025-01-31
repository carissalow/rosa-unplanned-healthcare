# Format data for number of significant symptom analysis:
# - Merge outcomes, predictors, and covariates
# - Determine if continuous predictor should be categorized and if so, define an appropriate cutpoint
# - Convert categorical predictors to factors with meaningful reference level
# - Restrict observations to those with complete outcome, predictor, and covariate data (i.e., complete cases)


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
daily_symptom_labels <- read_csv(here::here("data/processed/rosa_daily_symptom_labels_number_of_symptoms.csv"))


#### check distribution of number of symptoms to decide cutpoint for binning, if necessary ----

daily_symptom_labels %>%
  summarize(n = n(), .by = number_of_significant_individual_symptoms) %>%
  ggplot() +
  geom_bar(aes(x = number_of_significant_individual_symptoms, y = n), stat = "identity", fill = "grey80") +
  geom_text(aes(x = number_of_significant_individual_symptoms, y = n+150, label = scales::comma(n))) +
  labs(x = "Number of significant symptoms", y = "Frequency") +
  theme_light()

bin_number_of_symptoms <- TRUE
bin_number_of_symptoms_cutpoint <- 4 # selected based on visual inspection of plot above


#### create dataset for analysis ----

data_for_analysis <- healthcare_events %>%
  select(c(all_of(join_columns), all_of(outcome_columns))) %>%
  left_join(daily_symptom_labels, by = join_columns) 

# create binned version of number of significant symptoms due to sparsity
if (bin_number_of_symptoms) {
  
  if (bin_number_of_symptoms_cutpoint <= 0 | bin_number_of_symptoms_cutpoint %% 1 != 0) {
    stop("The specified cutpoint for binning the number of symptoms must be an integer greater than 0")
  }
  
  final_bin_label <- paste0(bin_number_of_symptoms_cutpoint, "+")
  bin_factor_levels <- c(as.character(seq(0, bin_number_of_symptoms_cutpoint - 1, by = 1)), final_bin_label)
  
  data_for_analysis <- data_for_analysis %>%
    mutate(
      number_of_significant_individual_symptoms_binned = case_when(
        number_of_significant_individual_symptoms >= bin_number_of_symptoms_cutpoint ~ final_bin_label,
        number_of_significant_individual_symptoms < bin_number_of_symptoms_cutpoint ~ as.character(number_of_significant_individual_symptoms),
        TRUE ~ NA_character_
      ),
      number_of_significant_individual_symptoms_binned = factor(
        number_of_significant_individual_symptoms_binned, 
        levels = bin_factor_levels
      )
    ) 
}

# drop observations with any missing data
if (restrict_to_complete_cases) {
  data_for_analysis <- data_for_analysis[complete.cases(data_for_analysis), ]
}
