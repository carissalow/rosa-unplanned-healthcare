# Figure 2 example: individual heatmap

library(tidyverse)
library(PNWColors)
library(patchwork)

source(here::here("src/figures/utils.R"))

pid <- 1043

#### import data ----

daily_symptom_highest_ratings <- read_csv(here::here("data/processed/rosa_daily_symptom_highest_ratings.csv"))
number_of_symptoms <- read_csv(here::here("data/processed/rosa_daily_symptom_labels.csv"))
healthcare_events <- read_csv(here::here("data/processed/rosa_combined_unplanned_healthcare_events_within_7day_windows.csv"))

pts_with_events <- healthcare_events %>% filter(unplanned_healthcare_event == 1) %>% pull(record_id) %>% unique()

#### prep symptom data ----

daily_symptom_highest_ratings <- daily_symptom_highest_ratings %>%
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

number_of_symptoms <- number_of_symptoms %>%
  select(record_id, date, study_day, number_of_moderate_to_severe_individual_symptoms) %>%
  mutate(
    number_of_moderate_to_severe_individual_symptoms_binned = case_when(
      number_of_moderate_to_severe_individual_symptoms >= 5 ~ "5+",
      is.na(number_of_moderate_to_severe_individual_symptoms) ~ "Unknown",
      TRUE ~ as.character(number_of_moderate_to_severe_individual_symptoms)
    ),
    number_of_moderate_to_severe_individual_symptoms_binned = factor(
      number_of_moderate_to_severe_individual_symptoms_binned, 
      levels = c("Unknown", "0", "1", "2", "3", "4", "5+")
    )
  ) 

#### plot UHE ----

event_plot <- healthcare_events %>%
  filter(record_id == pid) %>%
  ggplot() +
  geom_tile(
    aes(x = study_day, y = "UHE", fill = as.factor(unplanned_healthcare_event)), 
    color = "black"
  ) +
  scale_y_discrete(limits = rev, expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("white", "black"), drop = FALSE) +
  labs(
    x = "",
    y = "",
    fill = ""
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


#### plot symptom ratings  ----

symptom_rating_pal <- rev(PNWColors::pnw_palette("Bay", 8)[c(1:2, 5, 6, 8)])

symptom_rating_plot <- daily_symptom_highest_ratings %>%
  filter(record_id == pid) %>%
  ggplot() +
  geom_tile(
    aes(x = study_day, y = symptom, fill = highest_rating), color = "black"
  ) +
  scale_y_discrete(limits = rev, expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c(symptom_rating_pal, "grey50"), guide = guide_legend(reverse = TRUE), drop = FALSE) +
  labs(
    x = "Study day",
    y = "Symptom",
    fill = "Highest\nattribute\nrating"
  ) +
  theme_light() +
  theme(
    legend.position = "bottom",
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )

#### plot number of symptoms ----

number_of_symptoms_plot <- number_of_symptoms %>%
  filter(record_id == pid) %>%
  ggplot() +
  geom_tile(
    aes(x = study_day, y = "Number of symptoms", fill = number_of_moderate_to_severe_individual_symptoms_binned), color = "black"
  ) +
  scale_y_discrete(limits = rev, expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("grey50", rev(symptom_rating_pal), "darkred"), guide = guide_legend(nrow = 1), drop = FALSE) +
  labs(
    x = "Study day",
    y = "",
    fill = "Number of symptoms"
  ) +
  theme_light() +
  theme(legend.position = "bottom")
  
    
#### combine plots ----

event_plot / plot_spacer() / symptom_rating_plot / plot_spacer() / number_of_symptoms_plot + 
  plot_layout(heights = c(1, -1.1, 9, -1.1, 1))
    