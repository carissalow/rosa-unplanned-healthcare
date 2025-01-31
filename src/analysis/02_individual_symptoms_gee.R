# Fit a series of generalized estimating equations (GEE) to characterize the association between unplanned healthcare 
# encounters within 7 days and daily symptoms, operationalized as a moderate-to-severe rating for each individual 
# symptom at the day level. We first fit a series of univariable models, one for each predictor of interest and 
# each covariate separately, then for each univariable model for predictors of interest, we refit adjusting for 
# covariates


#### set-up ----

options(scipen = 999)

library(tidyverse)
library(geepack)

source(here::here("src/analysis/utils.R"))

alpha_level <- 0.05
conf_level <- 0.95

date_suffix <- gsub("-", "", as.character(Sys.Date()))


#### import data for analysis ----

source(here::here("src/analysis/01_format_individual_symptoms_data_for_analysis.R"))

id_variable <- "record_id"
outcome <- "unplanned_healthcare_event_within_7days_incl_today_with_unknowns"
covariates <- c("study_day")


#### individual moderate-to-severe symptom models ----

individual_symptom_predictors <- colnames(data_for_analysis)[grepl("symptom_day$", colnames(data_for_analysis))]

individual_symptom_predictor_list <- list(
  "predictors" = individual_symptom_predictors,
  "covariates" = covariates
)

individual_symptom_univariable_gee <- unlist(individual_symptom_predictor_list, use.names = FALSE) %>% 
  purrr::map(
    logistic_gee, 
    outcome = outcome, 
    id_variable = id_variable, 
    alpha_level = alpha_level,
    conf_level = conf_level,
    data = data_for_analysis
  ) %>% 
  purrr::reduce(bind_rows)

individual_symptom_covariate_adjusted_gee <- unlist(individual_symptom_predictor_list$predictors, use.names = FALSE) %>% 
  lapply(function(x) c(individual_symptom_predictor_list$covariates, x)) %>% 
  purrr::map(
    logistic_gee, 
    outcome = outcome, 
    id_variable = id_variable, 
    alpha_level = alpha_level,
    conf_level = conf_level,
    data = data_for_analysis
  ) %>% 
  purrr::reduce(bind_rows)

# save output
write_csv(individual_symptom_univariable_gee, here::here(glue::glue("output/results/rosa_unplanned_healthcare_event_individual_symptom_univariable_gee_results_{date_suffix}.csv")))
write_csv(individual_symptom_covariate_adjusted_gee, here::here(glue::glue("output/results/rosa_unplanned_healthcare_event_individual_symptom_covariate_adjusted_gee_results_{date_suffix}.csv")))
