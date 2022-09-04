# Setup
library(tidyverse)
library(scales)
library(ggsci)

# Load data 
crude_oil_production_annual <- read_csv("data/crude_oil_production_annual.csv")

# Wrangle data
# Count distinct number of countries/areas in data set
crude_oil_production_annual %>% distinct(crude_oil_production_annual$LOCATION) %>%
arrange() %>% tibble()

# Plot Crude oil production in Russia, compared to Ukraine, Saudi Arabia, Norway, and the EU
crude_oil_production_annual %>% 
  filter(LOCATION == "RUS" | LOCATION == "UKR" | LOCATION == "SAU" | LOCATION == "NOR" | LOCATION == "USA" | LOCATION == "EU28") %>%
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
       title = "Crude Oil Production (Total)",
       subtitle = "Measured in thousand tonne of oil equivalent (TOE)",
       caption = "Source: Extended World Energy Balances. OECD\nGraphic: @weiyuet")
  
# Save png
ggsave("figures/russia-crude-oil-production.png", width = 8, height = 6)