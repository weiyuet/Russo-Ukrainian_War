# Loading libraries
library(tidyverse)
library(scales)

# Loading data Russian casualties
russia_losses_personnel <- read_csv("data/russia_losses_personnel.csv")

russia_losses_personnel_long <-  russia_losses_personnel %>% 
  pivot_longer(cols = c(personnel, POW), names_to = "casualties", values_to = "value")

# Plot Russian casualties
russia_losses_personnel_long %>% 
  drop_na() %>% 
  ggplot(aes(x = date, y = value, colour = casualties)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  theme_classic() +
  theme(legend.position = c(0.8, 0.6)) +
  scale_colour_brewer(type = "qual", palette = 6) +
  labs(x = "", y = "", colour = "",
       title = "Russian Personnel Lost",
       subtitle = "Y-axis log scale",
       caption = "Source: Armed Forces of Ukraine, Ministry of Defense of Ukraine\nGraphic: @weiyuet")

ggsave("figures/russia-losses-personnel.png", width = 6, height = 4)
