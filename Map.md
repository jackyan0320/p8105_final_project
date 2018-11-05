Map
================

``` r
ny = map_data("state") %>% 
  filter(region == "new york")

ny_county = map_data("county")  %>% 
  filter(region == "new york")

ggplot() + geom_polygon(data = ny, aes(x=long, y = lat, group = group), fill = "gray") + 
  geom_polygon(data = ny_county, aes(x=long, y = lat, group = group), fill = NA, color = "white") +
  theme_bw()
```

![](Map_files/figure-markdown_github/unnamed-chunk-1-1.png)
