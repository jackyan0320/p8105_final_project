Mixed Model
================
Nick Williams
November 21, 2018

Random intercept models

``` r
no_sp_model <- organ_sp %>% 
  lmer(eligible_population_enrolled ~ total_days + opo + (1 | county), data = ., 
       REML = FALSE)

no_sp_model %>% 
  broom::tidy() %>% 
  filter(term == "total_days") %>% 
  mutate(estimate_year = estimate * 365, 
         se_year = std.error * 365, 
         ci_low = estimate_year - 1.96*se_year, 
         ci_upper = estimate_year + 1.96*se_year) %>% 
  mutate(term = ifelse(term == "total_days", "Time (year)", term)) %>% 
  select(term, estimate_year, se_year, ci_low, ci_upper) %>% 
  rename("Term" = term, 
         "$\\beta$" = estimate_year, 
         "Standard error" = se_year, 
         "Lower bound" = ci_low, 
         "Upper bound" = ci_upper) %>% 
  knitr::kable(digits = 3, escape = F) %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover", "striped"))
```

<table class="table table-hover table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Term
</th>
<th style="text-align:right;">
*β*
</th>
<th style="text-align:right;">
Standard error
</th>
<th style="text-align:right;">
Lower bound
</th>
<th style="text-align:right;">
Upper bound
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Time (year)
</td>
<td style="text-align:right;">
2.675
</td>
<td style="text-align:right;">
0.008
</td>
<td style="text-align:right;">
2.66
</td>
<td style="text-align:right;">
2.69
</td>
</tr>
</tbody>
</table>
``` r
organ_sp %>% 
  ggplot(aes(x = total_days, y = eligible_population_enrolled)) + 
  geom_jitter()
```

<img src="mixed_models_files/figure-markdown_github/overall time-1.png" width="75%" />

``` r
add_predictions(organ_sp, no_sp_model) %>% 
  ggplot(aes(x = total_days, y = pred, color = opo, type = county)) + 
  geom_line() + 
  geom_jitter(aes(x = total_days, y = eligible_population_enrolled), color = "black", alpha = 0.1) +
  viridis::scale_color_viridis(discrete = TRUE) + 
  theme(legend.position = "bottom")
```

<img src="mixed_models_files/figure-markdown_github/overall time-2.png" width="75%" />

``` r
sp_12_model <- organ_sp %>% 
  lmer(eligible_population_enrolled ~ total_days + year_sp_2012 + (1 | county), data = .)

sp_12_model %>% 
  broom::tidy() %>% 
  filter(term %in% c("total_days", "year_sp_2012")) %>% 
  mutate(estimate_year = estimate * 365, 
         se_year = std.error * 365, 
         ci_low = estimate_year - 1.96*se_year, 
         ci_upper = estimate_year + 1.96*se_year) %>% 
  mutate(term = ifelse(term == "total_days", "Time (year)", term), 
         term = ifelse(term == "year_sp_2012", "2012 spline", term)) %>% 
  select(term, estimate_year, se_year, ci_low, ci_upper) %>% 
  rename("Term" = term, 
         "$\\beta$" = estimate_year, 
         "Standard error" = se_year, 
         "Lower bound" = ci_low, 
         "Upper bound" = ci_upper) %>% 
  knitr::kable(digits = 3) %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover", "striped"))
```

<table class="table table-hover table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Term
</th>
<th style="text-align:right;">
*β*
</th>
<th style="text-align:right;">
Standard error
</th>
<th style="text-align:right;">
Lower bound
</th>
<th style="text-align:right;">
Upper bound
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Time (year)
</td>
<td style="text-align:right;">
2.883
</td>
<td style="text-align:right;">
0.022
</td>
<td style="text-align:right;">
2.840
</td>
<td style="text-align:right;">
2.926
</td>
</tr>
<tr>
<td style="text-align:left;">
2012 spline
</td>
<td style="text-align:right;">
-0.326
</td>
<td style="text-align:right;">
0.032
</td>
<td style="text-align:right;">
-0.389
</td>
<td style="text-align:right;">
-0.262
</td>
</tr>
</tbody>
</table>
``` r
add_predictions(organ_sp, sp_12_model) %>% 
  ggplot(aes(x = total_days, y = pred, color = opo, type = county)) + 
  geom_line() +
  viridis::scale_color_viridis(discrete = TRUE) + 
  theme(legend.position = "bottom")
```

<img src="mixed_models_files/figure-markdown_github/spline 2012-1.png" width="75%" />

``` r
sp_16_model <- organ_sp %>% 
  lmer(eligible_population_enrolled ~ total_days + year_sp_2016 + opo + (1 | county), data = .) 

sp_16_model %>% 
  broom::tidy() %>% 
  filter(term %in% c("total_days", "year_sp_2016")) %>% 
  mutate(estimate_year = estimate * 365, 
         se_year = std.error * 365, 
         ci_low = estimate_year - 1.96*se_year, 
         ci_upper = estimate_year + 1.96*se_year) %>% 
  mutate(term = ifelse(term == "total_days", "Time (year)", term), 
         term = ifelse(term == "year_sp_2016", "2016 spline", term)) %>% 
  select(term, estimate_year, se_year, ci_low, ci_upper) %>% 
  rename("Term" = term, 
         "$\\beta$" = estimate_year, 
         "Standard error" = se_year, 
         "Lower bound" = ci_low, 
         "Upper bound" = ci_upper) %>% 
  knitr::kable(digits = 3) %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover", "striped"))
```

<table class="table table-hover table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Term
</th>
<th style="text-align:right;">
*β*
</th>
<th style="text-align:right;">
Standard error
</th>
<th style="text-align:right;">
Lower bound
</th>
<th style="text-align:right;">
Upper bound
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Time (year)
</td>
<td style="text-align:right;">
2.563
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
2.542
</td>
<td style="text-align:right;">
2.583
</td>
</tr>
<tr>
<td style="text-align:left;">
2016 spline
</td>
<td style="text-align:right;">
0.789
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.692
</td>
<td style="text-align:right;">
0.886
</td>
</tr>
</tbody>
</table>
``` r
add_predictions(organ_sp, sp_16_model) %>% 
  ggplot(aes(x = total_days, y = pred, color = opo, type = county)) + 
  geom_line() + 
  viridis::scale_color_viridis(discrete = TRUE) + 
  theme(legend.position = "bottom")
```

<img src="mixed_models_files/figure-markdown_github/spline 2016-1.png" width="75%" />

``` r
sp_17_model <- organ_sp %>% 
  lmer(eligible_population_enrolled ~ total_days + year_sp_2017 + opo + (1 | county), data = ., 
       REML = FALSE)

sp_17_model %>% 
  broom::tidy() %>% 
  filter(term %in% c("total_days", "year_sp_2017")) %>% 
  mutate(estimate_year = estimate * 365, 
         se_year = std.error * 365, 
         ci_low = estimate_year - 1.96*se_year, 
         ci_upper = estimate_year + 1.96*se_year) %>% 
  mutate(term = ifelse(term == "total_days", "Time (year)", term), 
         term = ifelse(term == "year_sp_2017", "2017 spline", term)) %>% 
  select(term, estimate_year, se_year, ci_low, ci_upper) %>% 
  rename("Term" = term, 
         "$\\beta$" = estimate_year, 
         "Standard error" = se_year, 
         "Lower bound" = ci_low, 
         "Upper bound" = ci_upper) %>% 
  knitr::kable(digits = 3) %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover", "striped"))
```

<table class="table table-hover table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Term
</th>
<th style="text-align:right;">
*β*
</th>
<th style="text-align:right;">
Standard error
</th>
<th style="text-align:right;">
Lower bound
</th>
<th style="text-align:right;">
Upper bound
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Time (year)
</td>
<td style="text-align:right;">
2.561
</td>
<td style="text-align:right;">
0.009
</td>
<td style="text-align:right;">
2.543
</td>
<td style="text-align:right;">
2.579
</td>
</tr>
<tr>
<td style="text-align:left;">
2017 spline
</td>
<td style="text-align:right;">
1.700
</td>
<td style="text-align:right;">
0.078
</td>
<td style="text-align:right;">
1.548
</td>
<td style="text-align:right;">
1.852
</td>
</tr>
</tbody>
</table>
``` r
add_predictions(organ_sp, sp_17_model) %>% 
  ggplot(aes(x = total_days, y = pred, color = opo, type = county)) + 
  geom_line() +
  viridis::scale_color_viridis(discrete = TRUE) + 
  theme(legend.position = "bottom")
```

<img src="mixed_models_files/figure-markdown_github/spline 2017-1.png" width="75%" />

``` r
all_sp_model <- organ_sp %>% 
  lmer(eligible_population_enrolled ~ total_days + year_sp_2012 + year_sp_2016 + year_sp_2017 + opo + 
         (1 | county), data = ., REML = FALSE)

all_sp_model %>% 
  broom::tidy() %>% 
  filter(term %in% c("total_days", "year_sp_2012", "year_sp_2016", "year_sp_2017")) %>% 
  mutate(estimate_year = estimate * 365, 
         se_year = std.error * 365, 
         ci_low = estimate_year - 1.96*se_year, 
         ci_upper = estimate_year + 1.96*se_year) %>% 
  mutate(term = ifelse(term == "total_days", "Time (year)", term), 
         term = ifelse(term == "year_sp_2012", "2012 spline", term), 
         term = ifelse(term == "year_sp_2016", "2016 spline", term), 
         term = ifelse(term == "year_sp_2017", "2017 spline", term)) %>% 
  select(term, estimate_year, se_year, ci_low, ci_upper) %>% 
  rename("Term" = term, 
         "$\\beta$" = estimate_year, 
         "Standard error" = se_year, 
         "Lower bound" = ci_low, 
         "Upper bound" = ci_upper) %>% 
  knitr::kable(digits = 3) %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover", "striped")) %>% 
  footnote(general = "Splines correspond with dates in which 3 major policy changes were inacted. \nEstimates are adjusted for organ donor organization")
```

<table class="table table-hover table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Term
</th>
<th style="text-align:right;">
*β*
</th>
<th style="text-align:right;">
Standard error
</th>
<th style="text-align:right;">
Lower bound
</th>
<th style="text-align:right;">
Upper bound
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Time (year)
</td>
<td style="text-align:right;">
3.088
</td>
<td style="text-align:right;">
0.022
</td>
<td style="text-align:right;">
3.045
</td>
<td style="text-align:right;">
3.131
</td>
</tr>
<tr>
<td style="text-align:left;">
2012 spline
</td>
<td style="text-align:right;">
-0.982
</td>
<td style="text-align:right;">
0.041
</td>
<td style="text-align:right;">
-1.063
</td>
<td style="text-align:right;">
-0.902
</td>
</tr>
<tr>
<td style="text-align:left;">
2016 spline
</td>
<td style="text-align:right;">
0.333
</td>
<td style="text-align:right;">
0.150
</td>
<td style="text-align:right;">
0.039
</td>
<td style="text-align:right;">
0.627
</td>
</tr>
<tr>
<td style="text-align:left;">
2017 spline
</td>
<td style="text-align:right;">
2.465
</td>
<td style="text-align:right;">
0.213
</td>
<td style="text-align:right;">
2.048
</td>
<td style="text-align:right;">
2.883
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; border: 0;" colspan="100%">
<span style="font-style: italic;">Note: </span>
</td>
</tr>
<tr>
<td style="padding: 0; border: 0;" colspan="100%">
<sup></sup> Splines correspond with dates in which 3 major policy changes were inacted. <br>Estimates are adjusted for organ donor organization
</td>
</tr>
</tfoot>
</table>
``` r
add_predictions(organ_sp, all_sp_model) %>% 
  ggplot(aes(x = total_days, y = pred, color = opo, type = county)) + 
  geom_line() +
  viridis::scale_color_viridis(discrete = TRUE) + 
  geom_vline(xintercept = 1491, linetype = "dashed") + 
  geom_vline(xintercept = 2799, linetype = "dashed") + 
  geom_vline(xintercept = 3088, linetype = "dashed") + 
  annotate(geom = "text", x = 1720, y = 5, label = "2012") +
  annotate(geom = "text", x = 2600, y = 5, label = "2016") +
  annotate(geom = "text", x = 3300, y = 9, label = "2017") +
  theme(legend.position = "bottom", 
        legend.title = element_blank()) + 
  guides(color = guide_legend(ncol = 2)) + 
  labs(title = "Proportion of organ donors in NY counties", 
       x = "Days since start of data collection",
       y = "% enrolled as donors")
```

<img src="mixed_models_files/figure-markdown_github/all splines-1.png" width="75%" />

Comparing nested models

``` r
mods <- list(no_sp_model, sp_12_model, sp_16_model, sp_17_model, all_sp_model)

names(mods) <- c("no_sp_model", "sp_12_model", "sp_16_model", "sp_17_model", "all_sp_model")

rmse_fun <- function(model) {
  rmse(model, organ_sp)
}

purrr::map(mods, rmse_fun) %>% 
  as_tibble() %>% 
  gather(key = "model", 
         value = "rmse") %>% 
  mutate(model = ifelse(model == "no_sp_model", "No spline", model), 
         model = ifelse(model == "sp_12_model", "2012 spline", model), 
         model = ifelse(model == "sp_16_model", "2016 spline", model), 
         model = ifelse(model == "sp_17_model", "2017 spline", model), 
         model = ifelse(model == "all_sp_model", "All splines", model)) %>% 
  rename("Model" = model, 
         "RMSE" = rmse) %>% 
  knitr::kable(digits = 3) %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover", "striped"))
```

<table class="table table-hover table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Model
</th>
<th style="text-align:right;">
RMSE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
No spline
</td>
<td style="text-align:right;">
1.915
</td>
</tr>
<tr>
<td style="text-align:left;">
2012 spline
</td>
<td style="text-align:right;">
1.902
</td>
</tr>
<tr>
<td style="text-align:left;">
2016 spline
</td>
<td style="text-align:right;">
1.883
</td>
</tr>
<tr>
<td style="text-align:left;">
2017 spline
</td>
<td style="text-align:right;">
1.855
</td>
</tr>
<tr>
<td style="text-align:left;">
All splines
</td>
<td style="text-align:right;">
1.770
</td>
</tr>
</tbody>
</table>
``` r
options(knitr.kable.NA = '')

anova(no_sp_model, sp_17_model) %>% 
  broom::tidy() %>% 
  select(term, AIC, logLik, statistic, Chi.Df, p.value) %>% 
  mutate(term = ifelse(term == "no_sp_model", "No spline", term), 
         term = ifelse(term == "sp_17_model", "2017 spline", term), 
         p.value = ifelse(p.value < 0.001, "< 0.001", p.value)) %>% 
  knitr::kable(col.names = c("Model", "AIC", "Log-like", "$\\chi^2$", "df", "p-value"), 
               escape = F) %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover", "striped"))
```

<table class="table table-hover table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Model
</th>
<th style="text-align:right;">
AIC
</th>
<th style="text-align:right;">
Log-like
</th>
<th style="text-align:right;">
*χ*<sup>2</sup>
</th>
<th style="text-align:right;">
df
</th>
<th style="text-align:left;">
p-value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
No spline
</td>
<td style="text-align:right;">
30768.94
</td>
<td style="text-align:right;">
-15377.47
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
2017 spline
</td>
<td style="text-align:right;">
30306.86
</td>
<td style="text-align:right;">
-15145.43
</td>
<td style="text-align:right;">
464.084
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
&lt; 0.001
</td>
</tr>
</tbody>
</table>
``` r
anova(no_sp_model, all_sp_model) %>% 
  broom::tidy() %>% 
  select(term, AIC, logLik, statistic, Chi.Df, p.value) %>% 
  mutate(term = ifelse(term == "no_sp_model", "No spline", term), 
         term = ifelse(term == "all_sp_model", "All splines", term), 
         p.value = ifelse(p.value < 0.001, "< 0.001", p.value)) %>%
  knitr::kable(col.names = c("Model", "AIC", "Log-like", "$\\chi^2$", "df", "p-value"), 
               escape = F) %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover", "striped"))
```

<table class="table table-hover table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Model
</th>
<th style="text-align:right;">
AIC
</th>
<th style="text-align:right;">
Log-like
</th>
<th style="text-align:right;">
*χ*<sup>2</sup>
</th>
<th style="text-align:right;">
df
</th>
<th style="text-align:left;">
p-value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
No spline
</td>
<td style="text-align:right;">
30768.94
</td>
<td style="text-align:right;">
-15377.47
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
All splines
</td>
<td style="text-align:right;">
29630.95
</td>
<td style="text-align:right;">
-14805.47
</td>
<td style="text-align:right;">
1143.996
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
&lt; 0.001
</td>
</tr>
</tbody>
</table>
``` r
anova(sp_17_model, all_sp_model) %>% 
  broom::tidy() %>% 
  select(term, AIC, logLik, statistic, Chi.Df, p.value) %>% 
  mutate(term = ifelse(term == "sp_17_model", "2017 Spline", term), 
         term = ifelse(term == "all_sp_model", "All splines", term), 
         p.value = ifelse(p.value < 0.001, "< 0.001", p.value)) %>%
  knitr::kable(col.names = c("Model", "AIC", "Log-like", "$\\chi^2$", "df", "p-value"), 
               escape = F) %>% 
  kable_styling(full_width = F, bootstrap_options = c("hover", "striped"))
```

<table class="table table-hover table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Model
</th>
<th style="text-align:right;">
AIC
</th>
<th style="text-align:right;">
Log-like
</th>
<th style="text-align:right;">
*χ*<sup>2</sup>
</th>
<th style="text-align:right;">
df
</th>
<th style="text-align:left;">
p-value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2017 Spline
</td>
<td style="text-align:right;">
30306.86
</td>
<td style="text-align:right;">
-15145.43
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
All splines
</td>
<td style="text-align:right;">
29630.95
</td>
<td style="text-align:right;">
-14805.47
</td>
<td style="text-align:right;">
679.912
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
&lt; 0.001
</td>
</tr>
</tbody>
</table>
Random intercept and random slope models

``` r
# convergence issues with random slope, most likely not needed

# rs <- organ_sp %>% 
#   lmer(eligible_population_enrolled ~ total_days + (1 + total_days | county), data = .) 
# 
# coef(rs)$county %>% 
#   rownames_to_column(var = "county") %>% 
#   mutate(year = total_days * 365)
```
