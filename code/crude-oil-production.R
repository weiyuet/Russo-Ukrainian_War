# Setup
library(tidyverse)
library(scales)
library(ggsci)

# Load data 
crude_oil_production_annual <- read_csv("data/crude_oil_production_annual.csv")

# Count distinct number of countries/areas in data set
crude_oil_production_annual %>% distinct(crude_oil_production_annual$LOCATION) %>% count()

# Plot Crude oil production in Russia, compared to Ukraine, Saudi Arabia, Norway, the USA, and the EU
countries_to_include <- c("RUS", "UKR", "SAU", "NOR", "USA", "EU28")

crude_oil_production_annual %>% 
  filter(LOCATION %in% countries_to_include) %>%
  ggplot(aes(x = TIME, y = Value, colour = LOCATION)) +
  geom_line() +
  geom_point(size = 0.7) +
  scale_x_continuous(breaks = seq(1970, 2020, 5),
                     limits = c(1970, 2020)) +
  scale_y_log10(labels = label_number(suffix = " TOE", big.mark = ","),
                expand = c(0, 0),
                limits = c(1000, 1000000)) +
  theme_classic() +
  theme(legend.position = c(0.85, 0.4), 
        legend.title = element_blank(),
        axis.text.y = element_text(angle = 90)) +
  scale_colour_npg() +
  labs(x = "", y = "",
       title = "Crude Oil Production (Total Annual)",
       subtitle = "Measured in thousand tonne of oil equivalent (TOE) | y-axis log scale",
       caption = "Data: Extended World Energy Balances, OECD | Graphic: @weiyuet")
  
# Save image
ggsave("figures/russia-crude-oil-production.png", width = 8, height = 6)