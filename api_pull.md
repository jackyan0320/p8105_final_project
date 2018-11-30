
### Pulling data from API

This pulls all data from New York state's API for organ donor proportions per county from 2008 to 2018

``` r
organ <- GET("https://health.data.ny.gov/resource/km5a-7zrs.csv?$limit=10000") %>% 
  content("parsed") %>%
  janitor::clean_names() %>%
  filter(county != "Out of State" & county != "Unknown") %>%
  mutate(year = as.character(year)) %>%
  mutate(month = as.character(month)) %>%
  mutate(dummy_day = as.character("01")) %>%
  mutate(date = (str_c(year, month, dummy_day, sep = "-"))) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
  rename(pop_2012 = "x2012_census_population")
```

    ## Parsed with column specification:
    ## cols(
    ##   `_2012_census_population` = col_integer(),
    ##   chart_month = col_character(),
    ##   county = col_character(),
    ##   eligible_population_enrolled = col_double(),
    ##   location = col_character(),
    ##   month = col_integer(),
    ##   opo = col_character(),
    ##   population_18_estimate = col_integer(),
    ##   registry_enrollments = col_integer(),
    ##   year = col_integer()
    ## )

Creating spline

``` r
organ_sp <- organ %>% 
  mutate(year_sp_2012 = (date > "2012-10-1") * (date - as.Date("2012-10-1")), 
         year_sp_2016 = (date > "2016-5-1") * (date - as.Date("2016-5-1")), 
         year_sp_2017 = (date > "2017-2-14") * (date - as.Date("2017-2-14")),
         total_days = date - as.Date("2008-09-01"))
```

Exporting data for use in other files

``` r
write_csv(organ, "data/organ.csv")
write_csv(organ_sp, "data/organ_spline.csv")
```

Note that St. Lawrence County and Cattauragus county do not update their enrollment numbers until almost the end up the database time period/
