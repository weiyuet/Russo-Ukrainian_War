###########################
# 2022 Ukraine Russia War #
###########################

#### Setup ####
library(tidyverse)
library(scales)
library(glue)
library(paletteer)

######################
# Russian Casualties #
######################

#### Load data ####
russia_losses_personnel <- read_csv("data/russia_losses_personnel.csv")

#### Data to tidy format ####
russia_losses_personnel_tidy <- russia_losses_personnel %>%
  pivot_longer(cols = c(personnel, POW),
               names_to = "casualties",
               values_to = "value")

#### Visualize ####
russia_losses_personnel_tidy %>%
  drop_na() %>%
  ggplot(aes(x = date,
             y = value,
             colour = casualties)) +
  geom_step(linewidth = 1.1) +
  scale_x_date(date_breaks = "months",
               labels = label_date_short()) +
  scale_y_log10(breaks = c(0, 300, 1000, 3000, 10000, 30000, 100000),
                labels = label_number(big.mark = ",")) +
  scale_colour_paletteer_d("ggsci::default_jco",
                           labels = c("Personnel", "POWs")) +
  annotate(
    geom = "text",
    x = as.Date(glue("{max(russia_losses_personnel$date)}")),
    y = max(russia_losses_personnel$personnel) + 40000,
    label = glue("{max(russia_losses_personnel$personnel)}"),
    size = 3
  ) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.6)) +
  labs(
    x = "",
    y = "log scale",
    colour = "",
    shape = "",
    title = glue("Russian Casualties (Day {max(russia_losses_personnel$day)}, updated {max(russia_losses_personnel$date)})"),
    subtitle = "POW numbers not reported from 2022-04-27",
    caption = "Data: Armed Forces of Ukraine, Ministry of Defense of Ukraine via Kaggle | Graphic: @weiyuet"
  )

#### Save image ####
ggsave("figures/russia-losses-personnel.png", width = 7, height = 5)

#####################
# Russian Equipment #
#####################

#### Load data ####
russia_losses_equipment <- read_csv("data/russia_losses_equipment.csv")

#### Data to tidy format ####
russia_losses_equipment_tidy <- russia_losses_equipment %>%
  select(-"greatest losses direction") %>%
  pivot_longer(
    cols = c(aircraft:`cruise missiles`),
    names_to = "equipment", 
    values_to = "value"
  )

#### Visualize ####
russia_losses_equipment_tidy %>%
  ggplot(aes(x = date,
             y = value,
             colour = equipment)) +
  geom_step(colour = "gray35") +
  facet_wrap(~equipment,
             scales = "free") +
  scale_x_date(date_breaks = "2 months", 
               labels = label_date_short(),
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = label_number(big.mark = "",
                                           accuracy = 1)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(
    x = "",
    y = "",
    title = glue("Russian Equipment Lost (Day {max(russia_losses_equipment$day)}, updated {max(russia_losses_equipment$date)})"),
    caption = "Data: Armed Forces of Ukraine, Ministry of Defense of Ukraine via Kaggle | Graphic: @weiyuet"
  )

#### Save image ####
ggsave("figures/russia-losses-equipment.png", width = 8, height = 8)

################################
# Russian Equipment Cumulative #
################################

#### Visualize ####
russia_losses_equipment_tidy %>%
  group_by(equipment) %>%
  summarize(cumulative_total = max(value, na.rm = TRUE)) %>%
  mutate(equipment = fct_reorder(equipment, cumulative_total)) %>%
  ggplot(aes(x = cumulative_total,
             y = equipment)) +
  geom_col(colour = "gray10",
           fill = "gray35") +
  scale_x_continuous(
    labels = label_number(big.mark = ","),
    breaks = seq(0, 7000, 500),
    expand = c(0.01, 0)
  ) +
  theme_classic() +
  labs(
    x = "",
    y = "",
    title = glue("Russian Equipment Lost - Cumulative (Day {max(russia_losses_equipment$day)}, updated {max(russia_losses_equipment$date)})"),
    caption = "Data: Armed Forces of Ukraine, Ministry of Defense of Ukraine via Kaggle | Graphic: @weiyuet"
  )

#### Save image ####
ggsave("figures/russia-losses-equipment-cumulative.png", width = 8, height = 5)