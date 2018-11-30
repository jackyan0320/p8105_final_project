association\_demo\_registration
================
Jack Yan
11/20/2018

``` r
# load demo data
demo_df = read.csv("./data/ahrf_select_data.csv") %>% as.tibble()
# load registration data
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
