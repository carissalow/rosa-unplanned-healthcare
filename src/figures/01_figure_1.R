# Figure 1: Distribution of daily symptom maximum attribute ratings 

#### set-up ----

library(tidyverse)
library(PNWColors)
library(gt)

source(here::here("src/figures/utils.R"))


#### import and prep data ----

daily_symptom_labels <- read_csv(here::here("data/processed/rosa_daily_symptom_labels.csv"))
daily_symptom_highest_ratings <- read_csv(here::here("data/processed/rosa_daily_symptom_highest_ratings.csv"))

daily_symptoms_to_plot <- daily_symptom_labels %>%
  filter(!is.na(decreased_appetite_symptom_day)) %>%
  left_join(daily_symptom_highest_ratings, by = c("record_id", "date", "study_day")) %>%
  select(record_id, date, study_day, ends_with("highest_rating")) %>%
  mutate(
    across(
      ends_with("highest_rating"),
      function(x) case_when(
        is.na(x) ~ "Unknown",
        x %in% c(-1, 0) ~ "Not endorsed or 0",
        TRUE ~ as.character(x)
      )
    )
  ) %>%
  pivot_longer(
    cols = -c("record_id", "date", "study_day"),
    names_to = "symptom",
    names_transform = function(x) str_to_sentence(gsub("_", " ", gsub("_highest_rating", "", x))),
    values_to = "highest_rating"
  ) %>%
  mutate(
    highest_rating = factor(highest_rating, levels = rev(c("Unknown", "Not endorsed or 0", "1", "2", "3", "4")))
  )


#### create accompanying table ---- 

frequency_of_moderate_to_severe_symptoms <- daily_symptoms_to_plot %>%
  group_by(symptom, highest_rating) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(
    .by = symptom, 
    total_n = sum(n), 
    moderate_to_severe_n = sum(n[highest_rating %in% c("2", "3", "4")])
  ) %>%
  mutate(
    percent = round((moderate_to_severe_n/total_n)*100, 2),
    "Moderate-to-severe" = paste0(scales::comma(moderate_to_severe_n), " (", percent, "%)")
  ) %>%
  select(symptom, "Moderate-to-severe") %>%
  distinct() 

frequency_of_symptom_ratings <- daily_symptoms_to_plot %>%
  group_by(symptom, highest_rating) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(
    .by = symptom, 
    total_n = sum(n), 
  ) %>%
  mutate(
    percent = round((n/total_n)*100, 2),
    n_and_percent = paste0(scales::comma(n), " (", percent, "%)")
  )  %>%
  select(symptom, highest_rating, n_and_percent) %>%
  pivot_wider(
    names_from = highest_rating,
    values_from = n_and_percent
  ) %>%
  left_join(frequency_of_moderate_to_severe_symptoms, by = "symptom") %>%
  select(symptom, "Not endorsed or 0", "1", "2", "3", "4", "Moderate-to-severe")

table_for_figure_1 <- frequency_of_symptom_ratings %>%
  gt() %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels("symptom")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = "symptom")
  ) %>%
  tab_spanner(
    label = md("**Maximum attribute rating, *n* (%)**"),
    columns = -c("symptom",  "Moderate-to-severe")
  ) %>%
  cols_label(
    symptom = md("**Symptom**"),
    "Not endorsed or 0" = md("**Not endorsed or 0**"),
    "1" = md("**1**"),
    "2" = md("**2**"),
    "3" = md("**3**"),
    "4" = md("**4**"),
    "Moderate-to-severe" = md("**Moderate-to-severe,<br>*n* (%)**")
  )

gt::gtsave(table_for_figure_1, here::here("output/tables/table_for_figure_1.docx"))


#### create the plot ----

symptom_rating_pal <- rev(PNWColors::pnw_palette("Bay", 8)[c(1:2, 5, 6, 8)])

figure_1 <- daily_symptoms_to_plot %>%
  filter(highest_rating != "Unknown") %>%
  ggplot() +
  geom_bar(
    aes(y = symptom, fill = highest_rating), 
    position = "fill"
  ) +
  geom_text(
    aes(x = 0.01, y = symptom, label = symptom),
    color = "white", hjust = "left"
  ) +
  scale_y_discrete(limits = rev, expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), labels = scales::percent) +
  scale_fill_manual(values = symptom_rating_pal, guide = guide_legend(reverse = TRUE)) +
  coord_trans(x = "log10") +
  labs(
    x = "Percent of completed daily surveys",
    y = "Symptom",
    fill = "Highest attribute rating"
  ) +
  theme_light() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    # padding to prevent x-axis tick labels from being cut off 
    plot.margin = unit(c(.2, .5, .2, .2), "cm")
  )

ggsave(figure_1, filename = here::here("output/figures/figure_1.png"), dpi = 1000, width = 6, height = 7, units = "in")
