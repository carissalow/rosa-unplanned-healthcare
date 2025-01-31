# Create descriptive column names to replace abbreviated names in database

#### set-up ----

library(tidyverse)
library(yaml)

source(here::here("src/data/utils.R"))


#### pull data ----

group <- "ROSA"
credentials <- read_yaml(here::here("credentials.yaml"))
con <- connect_to_database(credentials = credentials, group = group)

daily_survey_metadata <- dbGetQuery(con, "select * from daily_responses_text_to_num_ref;")


#### format symptom column names ----

symptom_presence_column_names <- daily_survey_metadata %>%
  select(
    current_column_name = question_id, 
    new_column_name = response_text_val
  ) %>%
  filter(grepl("symptoms", current_column_name)) %>%
  mutate(
    new_column_name = case_when(
      grepl("[Dd]iarrhea", new_column_name) ~ "diarrhea",
      grepl("[Nn]umbness", new_column_name) ~ "neuropathy",
      TRUE ~ new_column_name
    ),
    new_column_name = str_to_lower(new_column_name),
    new_column_name = gsub(",.*", "", new_column_name),
    new_column_name = gsub(" \\(.*", "", new_column_name),
    new_column_name = gsub(" ", "_", new_column_name),
  ) 

symptom_severity_column_names <- daily_survey_metadata %>%
  select(current_column_name = question_id) %>%
  filter(!grepl("symptoms", current_column_name)) %>%
  distinct() %>%
  mutate(
    new_column_name = current_column_name %>%
      str_replace(., "app", "decreased_appetite") %>%
      str_replace(., "naus", "nausea") %>%
      str_replace(., "vomit", "vomiting") %>%
      str_replace(., "const", "constipation") %>%
      str_replace(., "diar", "diarrhea") %>%
      str_replace(., "painabd", "pain_in_the_abdomen") %>%
      str_replace(., "sob", "shortness_of_breath") %>%
      str_replace(., "neurop", "neuropathy") %>%
      str_replace(., "diz", "dizziness") %>%
      str_replace(., "ins", "insomnia") %>%
      str_replace(., "fatig", "fatigue") %>%
      str_replace(., "anx", "anxiety") %>%
      str_replace(., "sad", "sad_or_unhappy_feelings") %>%
      str_replace(., "conc", "problems_with_concentration") %>%
      str_replace(., "mem", "problems_with_memory") %>%
      str_replace(., "d_", "")
  )

daily_survey_column_name_mappings <- symptom_presence_column_names %>% 
  bind_rows(symptom_severity_column_names)

write_csv(daily_survey_column_name_mappings, here::here("data/processed/daily_survey_column_name_mappings.csv"))
