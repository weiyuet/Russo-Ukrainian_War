# Load libraries
library(tidyverse)
library(scales)

# Load data 
crude_oil_production_annual <- read_csv("data/crude_oil_production_annual.csv")

# Wrangle data
# Count distinct number of countries/areas
#list_of_areas <- crude_oil_production_annual %>% distinct(crude_oil_production_annual$LOCATION) %>% 
# arrange() %>% tibble()

# Plot Crude oil production in Russia, compared to Ukraine, Saudi Arabia, Norway, and the EU
crude_oil_production_annual %>% 
  filter(LOCATION == "RUS" | LOCATION == "UKR" | LOCATION == "SAU" | LOCATION == "NOR" | LOCATION == "USA" | LOCATION == "EU28") %>%
  ggplot(aes(x = TIME, y = Value, colour = LOCATION)) +
  geom_line() +
  geom_point(size = 0.65, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1965, 2020, 5),
                     expand = c(0, 0)) +
  scale_y_log10(labels = label_number(suffix = " TOE", big.mark = ",")) +
  theme_classic() +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        axis.text.y = element_text(angle = 90)) +
  scale_colour_brewer(type = "qual", palette = 2) +
  labs(x = "", y = "",
       title = "Crude Oil Production (Total)",
       subtitle = "Measured in thousand tonne of oil equivalent (TOE)",
       caption = "Source: Extended World Energy Balances. OECD\nGraphic: @weiyuet")
  
# Save png
ggsave("figures/russia-crude-oil-production.png", width = 6, height = 4)
