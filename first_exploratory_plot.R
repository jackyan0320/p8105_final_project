
library(httr)

organ <- GET("https://health.data.ny.gov/resource/km5a-7zrs.json") %>% 
  content("text") %>%
  jsonlite::fromJSON() 

library(tidyverse)

organ <- 
  read_csv("C:/Users/niwi8/OneDrive/Documents/fall_2018/data_science/final/data/Donate_Life_Organ_and_Tissue_Donor_Registry_Enrollment_by_County__Beginning_September_2008.csv") %>% 
  janitor::clean_names()

organ %>% 
  arrange(Year)


organ %>% 
  filter(month == 1, 
         county != "TOTAL NYS") %>% 
  ggplot(aes(x = year, y = percent_eligible_population_enrolled, color = county, size = x2012_census_population)) + 
  geom_point()
