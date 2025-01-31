# Table 3: Results of individual symptom models

#### set up ----

library(tidyverse)
library(gtsummary)
library(gt)

source(here::here("src/tables/utils.R"))

#### import data ----

unadjusted_results <- read_csv(here::here("output/results/rosa_unplanned_healthcare_event_individual_symptom_univariable_gee_results_20240220.csv"))
covariate_adjusted_results <- read_csv(here::here("output/results/rosa_unplanned_healthcare_event_individual_symptom_covariate_adjusted_gee_results_20240220.csv"))


#### prep data ----

excluded_predictors <- c("study_day")
join_columns <- c("n", "n_obs", "predictor", "level", "level_number", "n_per_level")

unadjusted_results_to_table <- format_results_to_table(unadjusted_results, excluded_predictors = excluded_predictors, adjusted = FALSE, include_q = TRUE, include_anova = FALSE)
covariate_adjusted_results_to_table <- format_results_to_table(covariate_adjusted_results, excluded_predictors = excluded_predictors, adjusted = TRUE, include_q = TRUE, include_anova = FALSE) 

results_to_table <- full_join(unadjusted_results_to_table, covariate_adjusted_results_to_table, by = join_columns)
  

#### create table ----

table_3 <- results_to_table %>%
  bind_rows(
    results_to_table %>%
      select(predictor, n_obs) %>%
      distinct() %>%
      mutate(level = predictor, level_number = 0)
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
  tab_stubhead(
    md("**Moderate-to-severe symptom**")
  ) %>%
  tab_stub_indent(
    rows = level_number != 0,
    indent = 3
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(rows = level_number == 0)
  ) %>%
  tab_spanner(
    label = md("**Unadjusted**"),
    columns = c("or_and_ci", "p_value", "q_value")
  ) %>%
  tab_spanner(
    label = md("**Covariate-adjusted**"),
    columns = paste0("adjusted_", c("or_and_ci", "p_value", "q_value"))
  ) %>%
  cols_hide(
    c(level_number, n, n_obs, predictor)
  ) %>%
  cols_label(
    n_per_level = md("**N**"),
    or_and_ci = md("**OR (95% CI)**"),
    p_value = md("***P* value**"),
    q_value = md("***Q* value**"),
    adjusted_or_and_ci = md("**OR (95% CI)**"),
    adjusted_p_value = md("***P* value**"),
    adjusted_q_value = md("***Q* value**"),
  ) %>%
  tab_header(
    md("**Table 3. Individual symptom model results**")
  ) %>%
  opt_align_table_header(
    align = "left"
  ) 

gt::gtsave(table_3, here::here("output/tables/table_3.docx"))
