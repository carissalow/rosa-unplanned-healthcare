# Table 4: Results of number of "significant" symptom models

#### set up ----

library(tidyverse)
library(gtsummary)
library(gt)

source(here::here("src/tables/utils.R"))

#### import data ----

unadjusted_results <- read_csv(here::here("output/results/rosa_unplanned_healthcare_event_number_of_significant_symptoms_univariable_gee_results_20240229.csv"))
covariate_adjusted_results <- read_csv(here::here("output/results/rosa_unplanned_healthcare_event_number_of_significant_symptoms_covariate_adjusted_gee_results_20240229.csv"))


#### prep data ----

excluded_predictors <- c("study_day")
join_columns <- c("n", "n_obs", "predictor", "level", "level_number", "n_per_level")

unadjusted_results_to_table <- format_gee_results_to_table(
  unadjusted_results, 
  excluded_predictors = excluded_predictors, 
  adjusted = FALSE, 
  include_q = FALSE, 
  include_anova = TRUE
)

covariate_adjusted_results_to_table <- format_gee_results_to_table(
  covariate_adjusted_results, 
  excluded_predictors = excluded_predictors, 
  adjusted = TRUE, 
  include_q = FALSE, 
  include_anova = TRUE
) 

results_to_table <- full_join(unadjusted_results_to_table, covariate_adjusted_results_to_table, by = join_columns)

#### create table ----

table_4 <- results_to_table %>%
  select(-contains("anova")) %>%
  bind_rows(
    results_to_table %>%
      select(predictor, n, n_obs, p_value = anova_p_value, adjusted_p_value = adjusted_anova_p_value) %>%
      distinct() %>%
      mutate(level = gsub("significant individual | binned$", "", predictor), level_number = 0)
  ) %>%
  arrange(predictor, level_number) %>%
  gt(
    rowname_col = "level"
  ) %>%
  fmt_integer(
    columns = n_per_level
  ) %>%
  fmt_markdown(
    columns = contains("or_and_ci")
  ) %>%
  sub_missing(
    missing_text = ""
  ) %>%
  #tab_stubhead(
  #  md("**Predictor**")
  #) %>%
  tab_stub_indent(
    rows = level_number != 0,
    indent = 3
  ) %>%
  tab_style(
    style = list(cell_text(align = "left")),
    locations = cells_stub(rows = TRUE)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(rows = level_number == 0)
  ) %>%
  tab_spanner(
    label = md("**Unadjusted**"),
    columns = c("or_and_ci", "p_value")
  ) %>%
  tab_spanner(
    label = md("**Covariate-adjusted**"),
    columns = paste0("adjusted_", c("or_and_ci", "p_value"))
  ) %>%
  cols_hide(
    c(level_number, n, n_obs, predictor)
  ) %>%
  cols_label(
    n_per_level = md("**N**"),
    or_and_ci = md("**OR (95% CI)**"),
    p_value = md("***P* value**"),
    adjusted_or_and_ci = md("**OR (95% CI)**"),
    adjusted_p_value = md("***P* value**"),
  ) %>%
  tab_header(
    md("**Table 4. Number of significant individual symptoms model results**")
  ) %>%
  opt_align_table_header(
    align = "left"
  ) 

gt::gtsave(table_4, here::here("output/tables/table_4.docx"))
