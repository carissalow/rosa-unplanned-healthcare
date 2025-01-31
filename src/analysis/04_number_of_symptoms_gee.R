# Fit a series of generalized estimating equations (GEE) to characterize the association between unplanned healthcare 
# encounters within 7 days and number of significant daily symptoms at the day level, operationalized as the total
# number of symptoms with a moderate-to-severe rating among those that were positively associated with UHE at least 
# at trend level (p<0.1) in the unadjusted and/or covariate-adjusted individual symptom models; the possible range 
# is 0-8 but 4+ was combined into a single category due to sparsity. We first fit a series of univariable models, 
# one for each predictor of interest and each covariate separately, then for each univariable model for predictors 
# of interest, we refit adjusting for covariates


#### set-up ----

options(scipen = 999)

library(tidyverse)
library(geepack)

source(here::here("src/analysis/utils.R"))

alpha_level <- 0.05
conf_level <- 0.95

date_suffix <- gsub("-", "", as.character(Sys.Date()))


#### import data for analysis ----

source(here::here("src/analysis/03_format_number_of_symptoms_data_for_analysis.R"))

id_variable <- "record_id"
outcome <- "unplanned_healthcare_event_within_7days_incl_today_with_unknowns"
covariates <- c("study_day")


#### number of moderate-to-severe symptoms models ----

number_of_symptoms_predictors <- c("number_of_significant_individual_symptoms_binned")

number_of_symptoms_predictor_list <- list(
  "predictors" = number_of_symptoms_predictors,
  "covariates" = covariates
)

number_of_symptoms_univariable_gee <- unlist(number_of_symptoms_predictor_list, use.names = FALSE) %>% 
  purrr::map(
    logistic_gee, 
    outcome = outcome, 
    id_variable = id_variable, 
    alpha_level = alpha_level,
    conf_level = conf_level,
    data = data_for_analysis
  ) %>% 
  purrr::reduce(bind_rows)

number_of_symptoms_covariate_adjusted_gee <- unlist(number_of_symptoms_predictor_list$predictors, use.names = FALSE) %>% 
  lapply(function(x) c(number_of_symptoms_predictor_list$covariates, x)) %>% 
  purrr::map(
    logistic_gee, 
    outcome = outcome, 
    id_variable = id_variable, 
    alpha_level = alpha_level,
    conf_level = conf_level,
    data = data_for_analysis
  ) %>% 
  purrr::reduce(bind_rows)

# number of moderate-to-severe individual symptoms among the following nine: constipation, decreased appetite, diarrhea, fatigue, nausea, pain, pain in the abdomen, SOB, vomiting
write_csv(number_of_symptoms_univariable_gee, here::here(glue::glue("output/results/rosa_unplanned_healthcare_event_number_of_significant_symptoms_univariable_gee_results_{date_suffix}.csv")))
write_csv(number_of_symptoms_covariate_adjusted_gee, here::here(glue::glue("output/results/rosa_unplanned_healthcare_event_number_of_significant_symptoms_covariate_adjusted_gee_results_{date_suffix}.csv")))
