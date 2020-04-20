Oil prices
================

  - [Packages](#packages)
  - [Clean raw data](#clean-raw-data)
      - [Supplement latest year with EIA oil
        prices](#supplement-latest-year-with-eia-oil-prices)
  - [Done, save](#done-save)

*Last updated on 20 April 2020*

## Packages

``` r
library("readxl")
library("ggplot2")
library("tidyr")
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
library("zoo")
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library("lubridate")
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     intersect, setdiff, union

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library("readr")
```

## Clean raw data

``` r
ff <- dir("input", full.names = TRUE, pattern = "bp-stats")

sheets  <- excel_sheets(ff)
sheet_n <- which(grepl("Oil \\- Crude prices since", sheets))

bp <- readxl::read_xlsx(ff, sheet = sheet_n, skip = 3)
bp <- bp %>%
  dplyr::filter(complete.cases(.)) %>%
  setNames(c("year", "drop", "oil_price_real")) %>%
  select(-drop) %>%
  mutate(year = as.integer(year))
```

### Supplement latest year with EIA oil prices

``` r
download.file("https://www.eia.gov/dnav/pet/hist_xls/RBRTEm.xls",
              "input/RBRTEm.xls")
eia <- read_xls("input/RBRTEm.xls", sheet = 2, skip = 2) %>%
  setNames(c("year", "brentm")) %>%
  mutate(year = as.integer(substr(year, 1, 4))) %>%
  group_by(year) %>%
  summarize(oil_price_real = mean(brentm))

# EIA oil prices are nominal, but since we only tacking on a year or two, don't 
# worry about it
oil <- bp %>%
  filter(year > 1950) %>%
  bind_rows(eia %>% filter(year > max(bp$year)))
```

## Done, save

``` r
write_csv(oil, "output/oil-prices.csv")
```
