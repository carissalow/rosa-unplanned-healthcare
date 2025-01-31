# Assign symptom labels to daily survey responses 

# For each completed daily survey response, we derive a series of labels reflecting a moderate-to-severe 
# individual symptom day (one per symptom). This is a binary indicator where a value of 1 is assigned if, 
# for the respective symptom, the participant endorsed experiencing the symptom and subsequently rated 
# one or more of its available attributes (severity, frequency, or interference) a 2 or higher on a 0-4 scale 
# (i.e, a rating of 2, 3, or 4) and a value of 0 is assigned otherwise. "None" or "other" symptoms are
# not considered, nor is "rash," which participants could only endorse and were not asked to rate.


#### set up ----

library(tidyverse)


#### helper functions ----

#' Calculate highest rating per symptom 
#' 
#' @param data Data frame containing at a minimum, record_id, date, and columns specified by `symptom_severity_cols`
#' @param symptom_severity_cols Vector of strings referencing names of symptom severity, frequency, and interference ratings in `data`; assumed to end with "_sev", "_freq", and "_int", respectively
#' @return Data frame containing columns for highest rating per symptom
calculate_highest_rating_per_symptom <- function(data, symptom_severity_cols = c()) {
  if (length(symptom_severity_cols) == 0){
    stop("You must pass a vector with at least one element to `symptom_severity_cols`")
  }
  
  symptom_severity_cols_groups <- unique(gsub("_sev$|_freq$|_int$", "", symptom_severity_cols))
  highest_symptom_rating_exprs <- glue::glue("invoke(pmax, c(across(all_of( grep('{symptom_severity_cols_groups}', symptom_severity_cols, value = TRUE) )), na.rm = TRUE))")
  names(highest_symptom_rating_exprs) <- glue::glue("{symptom_severity_cols_groups}_highest_rating")
  
  highest_symptom_ratings <- data %>%
    select(record_id, date, all_of(symptom_severity_cols)) %>%
    mutate(!!!rlang::parse_exprs(highest_symptom_rating_exprs)) %>%
    select(-symptom_severity_cols) 
  
  return(highest_symptom_ratings)
}

#### import data ----

daily_surveys <- read_csv(here::here("data/interim/daily_survey_responses_00-00-00_day_end_threshold_exclude_other_symptoms.csv"))
study_days <- read_csv(here::here("data/processed/rosa_study_dates_and_indexes.csv"))


#### assign indicators for moderate-to-severe (rating of 2, 3, or 4) individual symptoms  ----

identifier_cols <- c("record_id", "response_id", "date", "start_date", "progress", "duration_seconds")
symptom_severity_cols <- grep("_sev$|_int$|_freq$", names(daily_surveys), value = TRUE)
symptom_cols <- setdiff(names(daily_surveys), c(identifier_cols, symptom_severity_cols))

# find the max rating (0-4) among all available attributes for each symptom. Note that NA reflects
# that the symptom was not endorsed and thus did not receive a rating (effectively the same as but
# distinct from an endorsement and subsequent rating of 0).
# then create binary "symptom day" indicator for each symptom where 1 = max rating was a 2, 3, or 4 
# (corresponding to moderate severity or higher) and 0 = o.w.
moderate_to_severe_symptom_indicator_labels <- daily_surveys %>%
  calculate_highest_rating_per_symptom(
    symptom_severity_cols = symptom_severity_cols
  ) %>%
  mutate(
    across(
      ends_with("highest_rating"),
      function(x) ifelse(x %in% c(2, 3, 4), 1, 0),
      .names = "{gsub('_highest_rating', '_symptom_day', .col)}"
    )
  )


#### collect and save output ----

join_columns <- c("record_id", "date")

daily_symptom_data <- list(
    study_days, 
    moderate_to_severe_symptom_indicator_labels
  )  %>%
  purrr::reduce(
    left_join,
    by = join_columns
  ) 

# daily symptom labels for analysis
daily_symptom_labels <- daily_symptom_data %>%
  select(record_id, date, study_day, ends_with("symptom_day"))

write_csv(daily_symptom_labels, here::here("data/processed/rosa_daily_symptom_labels_individual.csv"))

# highest rating per symptom for plotting
# highest ratings are NA if the participant completed the survey but did not endorse and thus did 
# not rate the symptom; recode this as -1 to distinguish from missing data due to incomplete surveys 
daily_symptom_highest_ratings <- daily_symptom_data %>%
  select(record_id, date, study_day, ends_with("highest_rating")) %>%
  mutate(
    across(
      ends_with("highest_rating"), 
      function(x) ifelse(is.na(x), -1, x)
    )
  )
  
write_csv(daily_symptom_highest_ratings, here::here("data/processed/rosa_daily_symptom_highest_ratings.csv"))
