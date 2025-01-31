#### set up ----

library(tidyverse)

source(here::here("src/data/utils.R"))

min_pid <- 1001
max_pid <- 1183

#### import data ----

sean_sample_cancer_diagnoses_and_categories <- read_file_with_clean_names(here::here("data/raw/ROSA_Cancer_Categories.csv"))
liz_sample_cancer_diagnoses <- read_file_with_clean_names(here::here("data/raw/ROSA-EMR_DATA_2023-10-25_1651.csv"), excluded_column_patterns = "^chemo|^redcap")


#### combine data ----

liz_sample_cancer_diagnoses <- liz_sample_cancer_diagnoses %>%
  subset_participants("record_id", seq(min_pid, max_pid, by = 1)) %>%
  select(record_id, cancer_type_from_emr = cancer_type) %>%
  full_join(sean_sample_cancer_diagnoses_and_categories, by = c("record_id", "cancer_type_from_emr")) %>%
  rename(cancer_type_sean = cancer_type) %>%
  mutate(cancer_type_liz = "", notes = "")
 

#### save output ----

write_csv(liz_sample_cancer_diagnoses, here::here("data/interim/cancer_diagnoses_to_be_categorized.csv"), na = "")
