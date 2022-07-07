# Loading libraries
library(tidyverse)
library(scales)

# Loading data Russian casualties
russia_losses_personnel <- read_csv("data/russia_losses_personnel.csv")

# Data wrangling change data to long format
russia_losses_personnel_long <-  russia_losses_personnel %>% 
  pivot_longer(cols = c(personnel, POW), names_to = "casualties", values_to = "value")

# Plot Russian casualties
russia_losses_personnel_long %>% 
  drop_na() %>% 
  ggplot(aes(x = date, y = value, colour = casualties)) +
  geom_line() +
  geom_point() +
  scale_y_log10(labels = label_number(big.mark = ",")) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.6)) +
  scale_colour_brewer(type = "qual", palette = 6) +
  labs(x = "", y = "", colour = "",
       title = "Russian Personnel Lost",
       subtitle = "Y-axis log scale",
       caption = "Source: Armed Forces of Ukraine, Ministry of Defense of Ukraine\nGraphic: @weiyuet")

# Saving png
ggsave("figures/russia-losses-personnel.png", width = 6, height = 4)

# Loading data Russian equipment
russia_losses_equipment <- read_csv("data/russia_losses_equipment.csv")

# Data wrangling change data to long format
russia_losses_equipment_long <- russia_losses_equipment %>% 
  pivot_longer(cols = c(aircraft, helicopter, tank, APC, `field artillery`, drone, MRL, `fuel tank`, `naval ship`),
               names_to = "equipment", values_to = "value")

# Plot Russian equipment losses
russia_losses_equipment_long %>% 
  ggplot(aes(x = date, y = value, colour = equipment)) +
  geom_line(colour = "gray35") +
  facet_wrap(~equipment, scales = "free") +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "Russian Equipment Lost",
       caption = "Source: Armed Forces of Ukraine, Ministry of Defense of Ukraine\nGraphic: @weiyuet")

# Saving png
ggsave("figures/russia-losses-equipment.png", width = 6, height = 6)
