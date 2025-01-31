# Pull daily survey from database, assign surveys to dates based on provided time thresholds,
# de-duplicate responses, and clean deduplicated data

#### set up ----

library(tidyverse)
library(lubridate)
library(yaml)

source(here::here("src/data/utils.R"))


#### settings ----

# dB credentials
credentials <- read_yaml(here::here("credentials.yaml"))
group <- "ROSA"

# included participants
min_pid <- 1001
max_pid <- 1183

# minimum survey progress to be considered sufficiently complete
progress_threshold <- 50

# time thresholds for assigning survey responses to a date
# for e.g., 2 AM instead of midnight, change hours threshold from "00" to "02"
datetime_start_threshold <- "00:00:00"
datetime_end_threshold <- "00:00:00"

# settings for cleaning data
exclude_other_symptoms <- TRUE
use_descriptive_names <- TRUE
descriptive_name_mappings <- read_csv(here::here("data/interim/daily_survey_column_name_mappings.csv"))


#### pull and clean data ----

con <- connect_to_database(credentials = credentials, group = group)

daily_survey_data <- pull_daily_survey_data(con = con, pids = seq(min_pid, max_pid, by = 1)) %>%
  assign_daily_surveys_to_date(
    datetime_start_threshold = datetime_start_threshold, 
    datetime_end_threshold = datetime_end_threshold
  ) %>% 
  clean_and_deduplicate_daily_surveys(
    progress_threshold = progress_threshold, 
    use_descriptive_names = use_descriptive_names, 
    descriptive_name_mappings = descriptive_name_mappings, 
    exclude_other_symptoms = exclude_other_symptoms
  ) 
  
  
#### save cleaned data ----

day_end_threshold_string <- gsub("\\:", "-", datetime_end_threshold)
other_symptoms_string <- ifelse(exclude_other_symptoms, "exclude", "include")
output_file_path <- sprintf("data/interim/daily_survey_responses_%s_day_end_threshold_%s_other_symptoms.csv", day_end_threshold_string, other_symptoms_string)

write_csv(daily_survey_data, here::here(output_file_path))
