# Assign symptom labels to daily survey responses 

# For each completed daily survey response, we calculate a new variable reflecting the total
# number of moderate-to-severe individual symptoms (rating of 2, 3, or 4) among those that 
# were positively associated with UHE in either unadjusted univariable or covariate-adjusted 
# models, with statistical significance below specified (here, less conservative) threshold  


#### set-up ----

library(tidyverse)

unadjusted_results_file <- "output/results/rosa_unplanned_healthcare_event_individual_symptom_univariable_gee_results_20240220.csv"
covariate_adjusted_results_file <- "output/results/rosa_unplanned_healthcare_event_individual_symptom_covariate_adjusted_gee_results_20240220.csv"
  
p_value_threshold <- 0.1 # we use the threshold for "trend-level" to be less conservative
p_value_type <- "wald" # alternatively, "anova" (e.g., if categorical predictors have more than 2 levels)


#### import data ----

daily_symptom_labels <- read_csv(here::here("data/processed/rosa_daily_symptom_labels_individual.csv"))

individual_symptom_model_results_files <- list(unadjusted_results_file, covariate_adjusted_results_file)

individual_symptom_model_results <- individual_symptom_model_results_files %>%
  lapply(here::here) %>%
  purrr::map(read_csv) %>%
  purrr::reduce(bind_rows)


#### find symptoms associated with UHE with statistical significance below specified threshold ----

significant_symptoms <- individual_symptom_model_results %>%
  filter(
    grepl("symptom_day$", predictor) & 
    (or >= 1) & 
    (get(paste0(p_value_type, "_p_value")) < p_value_threshold)
  ) %>%
  pull(predictor) %>%
  unique() 


##### calculate number of "significant" symptoms ----

number_of_symptoms_labels <- daily_symptom_labels %>%
  mutate(
    number_of_significant_individual_symptoms = rowSums(select(., all_of(significant_symptoms)))
  ) %>%
  select(-ends_with("symptom_day"))


#### save output ----

write_delim(as.data.frame(gsub("_symptom_day$", "", significant_symptoms)), file = here::here("data/processed/significant_symptoms_included_in_number_of_symptoms_label.txt"), delim = "\n")
write_csv(number_of_symptoms_labels, file = here::here("data/processed/rosa_daily_symptom_labels_number_of_symptoms.csv"))
