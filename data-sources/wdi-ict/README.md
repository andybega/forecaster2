WDI: ICT
================

*Last updated on: 2020-04-23*

Cellphone and internet adoptions rates for all countries, 1960 on.

  - The WDI indicators used for this are …
  - The data were changed to conform as much as possible to the
    Gleditsch & Ward state list.
  - The data are lagged 1 year so that there are 2019 values
  - All missing values for series that were not completely missing for a
    country have been imputed. The strategy was:
      - …

<!-- end list -->

``` r
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
df <- read.csv("output/wdi-ict.csv")

glimpse(df)
```

    ## Rows: 9,321
    ## Columns: 6
    ## $ gwcode                     <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    ## $ year                       <int> 1960, 1961, 1962, 1963, 1964, 1965, 1966, …
    ## $ cellphones_per100          <dbl> 0.00000000, 0.00000000, 0.00000000, 0.0000…
    ## $ internet_users_pct         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ cellphones_per100_imputed  <int> 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, …
    ## $ internet_users_pct_imputed <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …

``` r
ggplot(df, aes(x = year, y = internet_users_pct, group = gwcode)) +
  geom_line(alpha = 0.5) +
  theme_light()
```

    ## Warning: Removed 20 rows containing missing values (geom_path).

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
# Scaled version that is adjusted for annual mean and sd
ggplot(df, aes(x = year, y = cellphones_per100, group = gwcode)) +
  geom_line(alpha = 0.5) +
  theme_light()
```

    ## Warning: Removed 10 rows containing missing values (geom_path).

![](README_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

## Data cleaning

See [clean-data.md](clean-data.md) for results of the data cleaning
script.
