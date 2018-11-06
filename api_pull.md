
### Pulling data from API

This pulls all data from New York state's API for organ donor proportions per county from 2008 to 2018

``` r
organ <- GET("https://health.data.ny.gov/resource/km5a-7zrs.csv?$limit=10000") %>% 
  content("parsed")
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
