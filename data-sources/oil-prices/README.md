Oil price
================

Brent crude oil price

The data are composed from two sources:

  - BP Statistical Review of World Energy for historical price
    information
    <https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy.html>
  - EIA Brent crude prices to supplement the last year of data

EIA oil prices are nominal while those from BP are real, but since we
are only adding EIA prices for the most recent year, I’m not worrying
about it.

## Summary

``` r
library("dplyr")
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
library("readr")

oil <- read_csv("output/oil-prices.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   year = col_double(),
    ##   oil_price_real = col_double()
    ## )

``` r
glimpse(oil)
```

    ## Rows: 70
    ## Columns: 2
    ## $ year           <dbl> 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, …
    ## $ oil_price_real <dbl> 16.51445, 16.16665, 18.10117, 18.01394, 18.08663, 17.8…
