Linear models
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)

set.seed(1)

#set the figures' size
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

#set the theme of all graphs
theme_set(theme_minimal() + theme(legend.position = "bottom"))

#set color of all graphs
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis" 
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

### Import data:

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(stars = review_scores_location / 2) %>% 
  rename(
    borough = neighbourhood_group,
    neighborhood = neighbourhood) %>% 
  filter(borough != "Staten Island") %>% 
  select(price, stars, borough, neighborhood, room_type)
```

### Fit a model:

``` r
nyc_airbnb %>% 
  ggplot(aes(x = stars, y = price, color = borough)) +
  geom_point()
```

    ## Warning: Removed 9962 rows containing missing values (geom_point).

<img src="Linear_models_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

``` r
# linear model: whether price as an outcome depends on rating and borough
fit = lm(price ~ stars + borough, data = nyc_airbnb)
```

Let’s look at the results:

Better way to look at the results:

``` r
broom::glance(fit) #format in datafram
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df   logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>
    ## 1    0.0342        0.0341  182.      271. 6.73e-229     4 -202113. 4.04e5 4.04e5
    ## # … with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
broom::tidy(fit) %>% # in alphabetical order, Boronx as reference group
  select(-std.error, -statistic) %>% 
  mutate(
    term = str_replace(term, "borough", "Borough:") # in `term`, change "borough" to "Borough:"
  ) %>% 
  knitr::kable(digits = 3)
```

| term              | estimate | p.value |
|:------------------|---------:|--------:|
| (Intercept)       |  -70.414 |   0.000 |
| stars             |   31.990 |   0.000 |
| Borough:Brooklyn  |   40.500 |   0.000 |
| Borough:Manhattan |   90.254 |   0.000 |
| Borough:Queens    |   13.206 |   0.145 |

### Be in control of factor:

1.  Change reference group:

``` r
# change to make the reference group be the most airbnb rentals

nyc_airbnb =
  nyc_airbnb %>% 
  mutate(
    borough = fct_infreq(borough), #change borough from character to factor
    room_type = fct_infreq(room_type)
  )
```

look at the plot again:

``` r
nyc_airbnb %>% 
  ggplot(aes(x = stars, y = price, color = borough)) +
  geom_point()
```

    ## Warning: Removed 9962 rows containing missing values (geom_point).

<img src="Linear_models_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

``` r
fit = lm(price ~ stars + borough, data = nyc_airbnb)

broom::tidy(fit)
```

    ## # A tibble: 5 × 5
    ##   term            estimate std.error statistic   p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         19.8     12.2       1.63 1.04e-  1
    ## 2 stars               32.0      2.53     12.7  1.27e- 36
    ## 3 boroughBrooklyn    -49.8      2.23    -22.3  6.32e-109
    ## 4 boroughQueens      -77.0      3.73    -20.7  2.58e- 94
    ## 5 boroughBronx       -90.3      8.57    -10.5  6.64e- 26

``` r
broom::glance(fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df   logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>
    ## 1    0.0342        0.0341  182.      271. 6.73e-229     4 -202113. 4.04e5 4.04e5
    ## # … with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

-   Hence, previously is comparing brooklyn to the bronx and mahattan to
    bronx…, now we are comparing brooklyn to manhattan and bronx to
    mahattan…
