library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(leaflet)
library(httr)

ny = map_data("state") %>% 
  filter(region == "new york")

ny_county = map_data("county")  %>% 
  filter(region == "new york")

organ <- GET("https://health.data.ny.gov/resource/km5a-7zrs.csv?$limit=10000") %>% 
  content("parsed") %>%
  janitor::clean_names() %>%
  filter(county != "TOTAL NYS" & county != "Out of State" & county != "Unknown") %>%
  mutate(year = as.character(year)) %>%
  mutate(month = as.character(month)) %>%
  mutate(dummy_day = as.character("01")) %>%
  mutate(date = (str_c(year, month, dummy_day, sep = "-"))) %>%
  mutate(date = as.Date(date, "%Y-%m-%d"))

organ_tidy = 
  organ %>% 
  separate(location, c("lat", "long"), sep = ",") %>% 
  mutate(long = str_replace(long, "\\)", ""),
         long = as.numeric(long)) %>% 
  mutate(lat = str_replace(lat, "\\(", ""),
         lat = as.numeric(lat))

ggplot() + 
  geom_polygon(data = ny, aes(x = long, y = lat, group = group), fill = "darkgrey") + 
  geom_polygon(data = ny_county, aes(x = long, y = lat, group = group), fill = NA, color = "white") +
  stat_density2d(data = organ_tidy, aes(x = long, y = lat, fill = ..level.., alpha = 0.25), size = 0.2, bins = 10, geom = "polygon") +
  geom_density2d(data = organ_tidy, aes(x = long, y = lat), size = 0.3) +
  coord_fixed(1.4) +
  theme_bw() +
  scale_fill_gradient(low = "light blue", high = "dark blue")