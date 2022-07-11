# Ukrainian Refugees

#Copy from [Tanya Shapiro](https://github.com/tashapiro/tanya-data-viz/blob/main/ukrainian-refugees/ukranian-refugees-map.R) as practice.
#Data from [UNHCR.](https://data.unhcr.org/en/situations/ukraine/location?secret=unhcrrestricted)


#import map dataframe from maps library
world_map <- map_data("world")

#https://data.unhcr.org/en/situations/ukraine/location?secret=unhcrrestricted
countries <- c("Ukraine", "Russian Federation", "Poland", "Republic of Moldova", "Romania", "Slovakia", "Hungary", "Belarus")
refugees <- c(0,1412429, 1194642, 82700, 83321, 79770, 25800, 9280)
data <- data.frame(countries, refugees)

#set fill colours per region. Ukraine = blue, Russia & Belarus = red, Neighbouring countries = green, Others = grey
world_map <- world_map %>% 
  mutate(fill = case_when(region == "Ukraine" ~ "#00A087",
                          region %in% c("Russia") ~ "#E64B35",
                          region %in% countries ~ "#4DBBD5",
                          TRUE ~ "grey85"))

#create dataframe for points of information
point_data <- world_map %>% 
  group_by(region) %>% 
  #create centroids for lat long per region
  filter(region %in% countries & region != "Ukraine") %>% 
  summarise(lat = mean(lat),
            long = mean(long)) %>% 
  #append refugee data
  left_join(data, by = c("region" = "countries")) %>% 
  mutate(
    #override Russia centroid, map cut-off
    lat = replace(lat, region == "Russia", 52),
    long = replace(long, region == "Russian", 39),
    #include Ukrainian label coordinates as starting point for migration lines
    lat_ukr = 49.25,
    long_ukr = 31.5,
    #add vjust parameters to adjust text data in plots
    region_vjust = case_when(refugees > 60000 ~ -.8, TRUE ~ -4),
    stat_vjust = case_when(refugees > 60000 ~1.1, TRUE ~ 0.1),
    name_colour = case_when(refugees > 60000 ~ "#000000", TRUE ~ "white")
  )

#Ukraine label
text_data <- data.frame(text = "UKRAINE",
                        lat = 49.25,
                        long = 31.5)

#import city coordinates from map library
data(world.cities)
#filter world cities for Ukranian cities, use top 5 most populous cities
ukr_cities <- world.cities %>% filter(country.etc == "Ukraine") %>% arrange(-pop) %>% head(5)

ggplot(world_map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = fill), colour = "white", size = 0.3) +
  #migration lines
  geom_segment(inherit.aes = FALSE, data = point_data, aes(x = long_ukr, xend = long, y = lat_ukr, yend = lat), size = 0.8, linetype = "dotted", colour = "#000000") +
  #overlay Ukrainian country map
  geom_polygon(data = world_map %>% filter(region == "Ukraine"), aes(fill = fill), colour = "white", size = 0.3) +
  #plot refugee bubbles
  geom_point(inherit.aes = FALSE, data = point_data, aes(long, lat, size = refugees/1000), fill = "gray85", colour = "#579DE2", shape = 21) +
  #overlay refugee numbers
  geom_text(inherit.aes = FALSE, data = point_data, aes(long, lat, label = scales::comma(refugees), vjust = stat_vjust), size = 2.5, family = "Arial", colour = "#000000") +
  #overlay country names
  geom_text(inherit.aes = FALSE, data = point_data, aes(long, lat, label = toupper(region), vjust = region_vjust, colour = name_colour), size = 2.8, family = "Arial") +
  #Ukraine label
  geom_text(inherit.aes = FALSE, data = text_data, aes(long, lat, label = text), size = 5.5, family = "Arial", colour = "#000000") +
  #Ukrainian cities
  geom_point(inherit.aes = FALSE, data = ukr_cities, aes(long, lat), colour = "#000000") +
  geom_text(inherit.aes = FALSE, data = ukr_cities, aes(long, lat, label = name), size = 4, vjust = 1.7, family = "Arial", colour = "#000000") +
  #set fill and colour to match inputs from dataframe with scale_identity
  scale_fill_identity() +
  scale_colour_identity() +
  scale_size(range = c(9, 37), breaks = c(100, 150, 200, 250, 500), guide = "none") +
  #adjust world map to focus on Ukraine and neighbouring countries with xlim & ylim
  coord_map(xlim = c(15, 42),
            ylim = c(44, 55)) +
  #add title and themes
  labs(title = "Number of Ukranian Refugees in Neighbouring Countries",
       caption = "Data from UNHCR as of 28 June, 2022\nGraphic: @weiyuet") +
  theme_void() +
  theme(text = element_text(family = "Arial"),
        plot.margin = margin(r = 15, l = 15),
        plot.title = element_text(size = 14, family = "Arial", margin = margin(b = 10)),
        plot.caption = element_text(size = 10)
  )

ggsave("figures/ukranian-refugees.png", width = 6, height = 4)