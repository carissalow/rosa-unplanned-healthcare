# Table 1: Sample characteristics 

#### set up ----

library(tidyverse)
library(gtsummary)
library(gt)

source(here::here("src/tables/utils.R"))
source(here::here("src/analysis/01_format_individual_symptoms_data_for_analysis.R"))

# include all participants in sample if TRUE or only those able to be analyzed if FALSE
include_all_participants <- TRUE


#### import data ----

if (include_all_participants) {
  participants_to_subset <- unique(daily_symptom_labels$record_id)
} else {
  participants_to_subset <- unique(data_for_analysis$record_id)
}

sample_characteristics_files <- list(
  study_dates = "data/raw/ROSA-ImportantDates_DATA_LABELS_2023-10-25_1635.csv",
  demographics = "data/raw/ROSA-Demographics_DATA_LABELS_2024-02-22_0852.csv",
  clinical = "data/raw/ROSA-EMR_DATA_2023-10-25_1651.csv",
  cancer_categories = "data/processed/rosa_cancer_diagnoses_categorized_by_liz_20240321.xlsx"
)

sample_characteristics_data <- sample_characteristics_files %>%
  purrr::map(read_file_with_clean_names, excluded_column_patterns = "redcap|^chemo|hosp|surgery|^ed$|^edvisit|[Rr]epeat") %>%
  purrr::reduce(full_join, by = "record_id") %>%
  subset_participants("record_id", participants_to_subset)


#### create new variables ----

sample_characteristics_data <- sample_characteristics_data %>%
  mutate(
    days_since_diagnosis_at_enrollment = as.numeric(study_start_date - cancer_dx_date, units = "days"),
    months_since_diagnosis_at_enrollment = lubridate::interval(cancer_dx_date, study_start_date) %/% months(1),
    cancer_type_liz = ifelse(cancer_type_liz == "GI", "Gastrointestinal", cancer_type_liz)
  ) 


#### format data ----

sample_characteristcs_data_for_table <- sample_characteristics_data %>%
  select(
    record_id,
    age_years = age,
    gender = what_is_your_gender,
    race_ethnicity = what_is_your_race_ethnicity,
    education = what_is_the_highest_level_of_education_that_you_completed,
    cancer_type = cancer_type_liz,
    cancer_stage,
    months_since_diagnosis_at_enrollment
  ) %>%
  # collapse cancer types with frequency <10 into single "Other" category
  mutate(
    .by = cancer_type,
    cancer_type_with_other_category = ifelse(n() < 10, "Other", cancer_type)
  ) %>%
  mutate(
    across(
      any_of(c("gender", "race_ethnicity", "education", "cancer_type")), 
      function(x) ifelse(is.na(x), "Unknown", x)
    ),
    race_ethnicity = factor(race_ethnicity, levels = c("Asian", "Black/African American", "White/Caucasian", "More than one race/ethnicity", "Other")),
    education = factor(
      education, 
      levels = c(
        "Less than a high school diploma", "High school diploma or equivalent", "Associate of arts or other 2-year degree", 
        "Some college, no degree", "Bachelor's degree", "Graduate degree", "Unknown"
        )
      ),
    cancer_stage = case_when(
      cancer_stage == "3 or 4" ~ "4",
      is.na(cancer_stage) ~ "Unknown",
      TRUE ~ cancer_stage
    ),
    cancer_type = factor(cancer_type),
    cancer_type_with_other_category = factor(
      cancer_type_with_other_category, 
      levels = c(sort(unique(.$cancer_type_with_other_category[!(.$cancer_type_with_other_category %in% c("Other"))])), "Other")
    ),
    cancer_stage = factor(cancer_stage, levels = c("0", "1", "2", "3", "4", "Unknown"))
  ) %>%
  relocate("cancer_type_with_other_category", .after = "cancer_type")


#### create table 1 ----

table_1 <- sample_characteristcs_data_for_table %>%
  tbl_summary(
    include = -c("record_id", "cancer_type"),
    include = -c("record_id", "cancer_type_with_other_category"),
    label = list(
      age_years ~ "Age, years",
      gender ~ "Gender",
      race_ethnicity ~ "Race",
      education ~ "Highest level of education",
      cancer_type_with_other_category ~ "Cancer type",
      cancer_type ~ "Cancer type",
      cancer_stage ~ "Cancer stage",
      months_since_diagnosis_at_enrollment ~ "Time since cancer diagnosis at enrollment, months"
    ),
    type = list(
      all_dichotomous() ~ "categorical"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) [{min}, {max}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(2, 2, 2, 2),
      c(age_years, months_since_diagnosis_at_enrollment) ~ c(2, 2, 0, 0),
      all_categorical() ~ c(0, 1)
    )
  ) %>%
  bold_labels() %>%
  as_gt() %>%
  gt::opt_align_table_header(
    align = "left"
  ) %>%
  gt::tab_header(
    md("**Table 1. Participant characteristics**")
  ) 

gt::gtsave(table_1, here::here("output/tables/table_1.docx"))


#### create table 1 with comparison of participants who did and did not have one or more UHEs ----- 

participants_with_uhe <- healthcare_events %>%
  subset_participants("record_id", participants_to_subset) %>%
  filter(!is.na(visit_category)) %>%
  pull(record_id) %>%
  unique()

table_1_w_comparison <- sample_characteristcs_data_for_table %>%
  mutate(
    had_uhe = ifelse(record_id %in% unique(participants_with_uhe), "Yes", "No"),
    had_uhe = factor(had_uhe, levels = c("Yes", "No")) 
  ) %>%
  tbl_summary(
    by = "had_uhe",
    include = -c("record_id", "cancer_type"),
    include = -c("record_id", "cancer_type_with_other_category"),
    label = list(
      age_years ~ "Age, years",
      gender ~ "Gender",
      race_ethnicity ~ "Race",
      education ~ "Highest level of education",
      cancer_type_with_other_category ~ "Cancer type",
      cancer_type ~ "Cancer type",
      cancer_stage ~ "Cancer stage",
      months_since_diagnosis_at_enrollment ~ "Time since cancer diagnosis at enrollment, months"
    ),
    type = list(
      all_dichotomous() ~ "categorical"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) [{min}, {max}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(2, 2, 2, 2),
      c(age_years, months_since_diagnosis_at_enrollment) ~ c(2, 2, 0, 0),
      all_categorical() ~ c(0, 1)
    )
  ) %>%
  add_overall() %>%
  add_p(
    test = list(c("cancer_type") ~ "fisher.test.simulate.p.values")
  ) %>%
  modify_fmt_fun(
    update = p.value ~ format_p_values
  ) %>%
  modify_header(
    p.value = "***P* value**"
  ) %>%
  modify_spanning_header(
    c(stat_1, stat_2) ~ "**Unplanned healthcare encounter**"
  ) %>%
  bold_labels() %>%
  as_gt() %>%
  gt::opt_align_table_header(
    align = "left"
  ) %>%
  gt::tab_header(
    md("**Table 1. Participant characteristics**")
  ) 

gt::gtsave(table_1_w_comparison, here::here("output/tables/table_1_w_comparison.docx"))