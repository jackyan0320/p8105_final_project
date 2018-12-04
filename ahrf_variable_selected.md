ahrf\_variable\_selected
================
Jack Yan
11/19/2018

select ahrf variables
---------------------

### Explore variables

``` r
read_csv("./data/ahrf_variables.csv") %>% 
  filter(str_detect(label, "Code") == FALSE) %>% 
  filter(str_detect(label, "M.D.") == F) %>% 
  filter(str_detect(label, "MD") == F) 
```

    ## Parsed with column specification:
    ## cols(
    ##   label = col_character()
    ## )

    ## # A tibble: 6,358 x 1
    ##    label                     
    ##    <chr>                     
    ##  1 Blank                     
    ##  2 Entity of File            
    ##  3 Secondary Entity Of File  
    ##  4 Date of File              
    ##  5 Date of Creation          
    ##  6 File Length               
    ##  7 State Name                
    ##  8 State Name Abbreviation   
    ##  9 County Name               
    ## 10 County Name w/State Abbrev
    ## # ... with 6,348 more rows

``` r
# Screen for variable names of interest
read_csv("./data/ahrf_variables.csv") %>%
  filter(str_detect(label, "Code") == FALSE) %>% 
  filter(str_detect(label, "File") == FALSE) %>% 
  filter(str_detect(label, "Name") == FALSE) %>% 
  mutate(truncated = str_trunc(label, 15)) %>%
  group_by(truncated) %>% nest # %>%  View
```

    ## Parsed with column specification:
    ## cols(
    ##   label = col_character()
    ## )

    ## # A tibble: 973 x 2
    ##    truncated       data             
    ##    <chr>           <list>           
    ##  1 Blank           <tibble [1 x 1]> 
    ##  2 Date of Crea... <tibble [1 x 1]> 
    ##  3 CBSA County ... <tibble [1 x 1]> 
    ##  4 Contiguous C... <tibble [14 x 1]>
    ##  5 Phys,Primary... <tibble [12 x 1]>
    ##  6 MD's,Primary... <tibble [12 x 1]>
    ##  7 DO's,Primary... <tibble [12 x 1]>
    ##  8 Tot Active M... <tibble [3 x 1]> 
    ##  9 Total Active... <tibble [22 x 1]>
    ## 10 Total M.D.'s... <tibble [35 x 1]>
    ## # ... with 963 more rows

### select variable names of interest

``` r
# Standardzd PerCapita Medcr Cost Fee
variables_list = 
  read_csv("./data/ahrf_variables.csv") %>%
  filter(str_detect(label, "Medcr Cost Fee") == TRUE|
         str_detect(label, "Medicare Enrollment") == TRUE) %>% 
  mutate(topic = "Medical Cost & Medicare Enrollment")
```

    ## Parsed with column specification:
    ## cols(
    ##   label = col_character()
    ## )

``` r
# Population Estimate & Census Population
variables_list = 
  read_csv("./data/ahrf_variables.csv") %>%
  filter(str_detect(label, "Population Estimate") == TRUE|
          str_detect(label, "Census Population 20") == TRUE,
          str_detect(label, "Veteran") == FALSE,
          str_detect(label, "65+") == FALSE) %>% 
  mutate(topic = "Population Size") %>%   
  bind_rows(variables_list, .)
```

    ## Parsed with column specification:
    ## cols(
    ##   label = col_character()
    ## )

``` r
# Gender & Ethnicity
variables_list = 
  read_csv("./data/ahrf_variables.csv") %>% 
  mutate(no = 1: length(label)) %>% 
  filter(no %in% 4224:4687) %>% 
  mutate(topic = "Gender & Ethnicity") %>% 
  select(-no) %>% 
  bind_rows(variables_list, .)
```

    ## Parsed with column specification:
    ## cols(
    ##   label = col_character()
    ## )

``` r
# Age
variables_list = 
  read_csv("./data/ahrf_variables.csv") %>%
  filter(str_detect(label, "Median Age") == TRUE) %>% 
  mutate(topic = "Age") %>% 
  bind_rows(variables_list, .)
```

    ## Parsed with column specification:
    ## cols(
    ##   label = col_character()
    ## )

``` r
# Education
variables_list = 
  read_csv("./data/ahrf_variables.csv") %>%
  filter(str_detect(label, "Percent Educ/HlthCare/Soc Asst") == TRUE|
          str_detect(label, "Pers 25+") == TRUE) %>% 
  mutate(topic = "Education") %>% 
  bind_rows(variables_list, .)
```

    ## Parsed with column specification:
    ## cols(
    ##   label = col_character()
    ## )

``` r
variables_list %>% 
  write_csv(path = "./data/ahrf_selected_variables.csv")
```

``` r
# Standardzd PerCapita Medcr Cost Fee
variables_list = 
  read_csv("./data/ahrf_variables.csv") %>%
  filter(str_detect(label, "Medcr Cost Fee") == TRUE|
         str_detect(label, "Medicare Enrollment") == TRUE) %>% 
  mutate(topic = "Medical Cost & Medicare Enrollment") %>% 
  filter(label == "Standardzd PerCapita Medcr Cost Fee for Service 2015"|
         label == 'Medicare Enrollment, Aged Tot 2015')
```

    ## Parsed with column specification:
    ## cols(
    ##   label = col_character()
    ## )

``` r
# Population Estimate & Census Population
variables_list = 
  read_csv("./data/ahrf_variables.csv") %>%
  filter(str_detect(label, "Population Estimate") == TRUE|
          str_detect(label, "Census Population 20") == TRUE,
          str_detect(label, "Veteran") == FALSE,
          str_detect(label, "65+") == FALSE) %>% 
  mutate(topic = "Population Size") %>%   
  filter(label == "Population Estimate 2016") %>% 
  bind_rows(variables_list, .)
```

    ## Parsed with column specification:
    ## cols(
    ##   label = col_character()
    ## )

``` r
# Gender & Ethnicity
variables_list = 
  read_csv("./data/ahrf_variables.csv") %>% 
  mutate(no = 1:length(label)) %>% 
  filter(no %in% 4224:4687) %>% 
  mutate(topic = "Gender & Ethnicity") %>% 
  select(-no) %>%
  filter(str_detect(label, "2015")) %>% 
  filter(label %in% c('Pop Total Male 2015','Pop Total Female 2015', 'Pop White Male 2015', 'Pop White Female 2015', 'Pop White Hispanic/Latino Male 2015', 'Pop Black/African Amer Male 2015', 'Pop Black/African Amer Female 2015', 'Pop Asian Male 2015', 'Pop Asian Female 2015' ))  %>% 
  bind_rows(variables_list, .)
```

    ## Parsed with column specification:
    ## cols(
    ##   label = col_character()
    ## )

``` r
# Age
variables_list =
  read_csv("./data/ahrf_variables.csv") %>%
  filter(str_detect(label, "Median Age") == TRUE) %>% 
  mutate(topic = "Age") %>% 
  filter(str_detect(label, "2010")) %>% 
    filter(label %in% c("Median Age 2010")) %>% 
  bind_rows(variables_list, .)
```

    ## Parsed with column specification:
    ## cols(
    ##   label = col_character()
    ## )

``` r
# Education
variables_list =
  read_csv("./data/ahrf_variables.csv") %>% 
  mutate(topic = "Education") %>%
  filter(label %in% c("Percent Educ/HlthCare/Soc Asst 2011-15", "% Persons 25+ w/<HS Diploma 2011-15", '% Persons 25+ w/4+ Yrs College 2011-15' )) %>% 
  bind_rows(variables_list, .)
```

    ## Parsed with column specification:
    ## cols(
    ##   label = col_character()
    ## )

``` r
variables_list %>% 
  write_csv(path = "./data/ahrf_selected_variables.csv")
```

``` r
read_csv("./data/ahrf_selected_variables.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   label = col_character(),
    ##   topic = col_character()
    ## )

    ## # A tibble: 16 x 2
    ##    label                                       topic                      
    ##    <chr>                                       <chr>                      
    ##  1 Standardzd PerCapita Medcr Cost Fee for Se~ Medical Cost & Medicare En~
    ##  2 Medicare Enrollment, Aged Tot 2015          Medical Cost & Medicare En~
    ##  3 Population Estimate 2016                    Population Size            
    ##  4 Pop Total Male 2015                         Gender & Ethnicity         
    ##  5 Pop Total Female 2015                       Gender & Ethnicity         
    ##  6 Pop White Male 2015                         Gender & Ethnicity         
    ##  7 Pop White Female 2015                       Gender & Ethnicity         
    ##  8 Pop White Hispanic/Latino Male 2015         Gender & Ethnicity         
    ##  9 Pop Black/African Amer Male 2015            Gender & Ethnicity         
    ## 10 Pop Black/African Amer Female 2015          Gender & Ethnicity         
    ## 11 Pop Asian Male 2015                         Gender & Ethnicity         
    ## 12 Pop Asian Female 2015                       Gender & Ethnicity         
    ## 13 Median Age 2010                             Age                        
    ## 14 % Persons 25+ w/<HS Diploma 2011-15         Education                  
    ## 15 % Persons 25+ w/4+ Yrs College 2011-15      Education                  
    ## 16 Percent Educ/HlthCare/Soc Asst 2011-15      Education
