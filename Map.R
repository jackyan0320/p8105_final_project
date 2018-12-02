library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(leaflet)
library(httr)
library(plotly)

# use the API to pull data

organ <- GET("https://health.data.ny.gov/resource/km5a-7zrs.csv?$limit=10000") %>% 
  content("parsed") %>%
  janitor::clean_names() %>%
  filter(county != "TOTAL NYS" & county != "Out of State" & county != "Unknown") %>%
  mutate(year = as.character(year)) %>%
  mutate(month = as.character(month)) %>%
  mutate(dummy_day = as.character("01")) %>%
  mutate(date = (str_c(year, month, dummy_day, sep = "-"))) %>%
  mutate(date = as.Date(date, "%Y-%m-%d"))

# Tidy the data for plot the donation map

organ_tidy = 
  organ %>% 
  separate(location, c("lat", "long"), sep = ",") %>% 
  mutate(long = str_replace(long, "\\)", ""),
         long = as.numeric(long)) %>% 
  mutate(lat = str_replace(lat, "\\(", ""),
         lat = as.numeric(lat)) 

ny = map_data("state") %>% 
  filter(region == "new york")

ny_county = map_data("county")  %>% 
  filter(region == "new york") %>% 
  as.tibble() %>% 
  rename(county = subregion)

organ_df = 
  organ_tidy %>% 
  select(eligible_population_enrolled, county) %>% 
  mutate(county = tolower(county)) %>% 
  mutate(county = recode(county,  'cattauragus' = 'cattaraugus'))

ny_county_combined = 
  full_join(organ_df, ny_county, by = 'county')

df_1 = organ_df %>% distinct(county) 
df_2 = ny_county %>% distinct(county) 


# Mapping

p = 
  ggplot() + 
  geom_polygon(data = ny_county_combined, 
               aes(x = long, 
                   y = lat, 
                   group = group, 
                   fill = eligible_population_enrolled)) +
  geom_path(data = ny_county_combined, 
            aes(x = long, 
                y = lat,
                group = group), 
            color = "white", 
            size = 0.1) +
  geom_text(data = organ_tidy, 
            aes(x = long, 
                y = lat, 
                label = county),
            size = 2.5,
            color = "white") +
  labs(x = 'Longitude', y = 'Latitude', title = 'map', fill = '% Enrollment') +
  coord_equal() +
  viridis::scale_fill_viridis(option = "magma", direction = -1)

ggplotly(p)