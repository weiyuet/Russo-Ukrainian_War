# Setup
library(tidyverse)
library(scales)
library(glue)

# Load data Russian casualties
russia_losses_personnel <- read_csv("data/russia_losses_personnel.csv")

# Wrangle data change data to long format
russia_losses_personnel_long <- russia_losses_personnel %>%
  pivot_longer(cols = c(personnel, POW), names_to = "casualties", values_to = "value")

# Plot Russian casualties
russia_losses_personnel_long %>%
  drop_na() %>%
  ggplot(aes(x = date, y = value, colour = casualties)) +
  geom_line() +
  geom_point(aes(shape = casualties), size = 0.7) +
  scale_y_log10(labels = label_number(big.mark = ",")) +
  scale_x_date(date_breaks = "1 month", labels = label_date_short()) +
  annotate(geom = "text",
           x = as.Date(glue("{max(russia_losses_personnel_long$date)}")),
           y = max(russia_losses_personnel$personnel) + 15000, label = glue("{max(russia_losses_personnel$personnel)}"), size = 3) + # peak
  theme_classic() +
  theme(legend.position = c(0.8, 0.55)) +
  scale_colour_grey(start = 0.2, end = 0.45) +
  labs(x = "", y = "",
    colour = "", shape = "",
    title = "Russian Casualties",
    caption = "Source: Armed Forces of Ukraine, Ministry of Defense of Ukraine\nGraphic: @weiyuet")

# Save png
ggsave("figures/russia-losses-personnel.png", width = 6, height = 4)

# Load data Russian equipment
russia_losses_equipment <- read_csv("data/russia_losses_equipment.csv")

# Wrangle data change data to long format
russia_losses_equipment_long <- russia_losses_equipment %>%
  select(-"greatest losses direction") %>%
  pivot_longer(cols = c(aircraft:`cruise missiles`),
    names_to = "equipment", values_to = "value")

# Plot Russian equipment losses
russia_losses_equipment_long %>%
  ggplot(aes(x = date, y = value, colour = equipment)) +
  geom_line(colour = "gray35") +
  facet_wrap(~equipment, scales = "free") +
  scale_x_date(date_breaks = "2 month", labels = label_date_short()) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
    title = "Russian Equipment Lost",
    caption = "Source: Armed Forces of Ukraine, Ministry of Defense of Ukraine\nGraphic: @weiyuet")

# Save png
ggsave("figures/russia-losses-equipment.png", width = 8, height = 8)