Ingest WDI ICT (Internet and mobile use)
================

  - [Packages / functions](#packages-functions)
  - [Explore possible indicators](#explore-possible-indicators)
  - [Get raw data](#get-raw-data)
  - [Clean raw data](#clean-raw-data)
      - [Lag data](#lag-data)
      - [Normalize to G\&W statelist](#normalize-to-gw-statelist)
  - [Handle missing values](#handle-missing-values)
      - [Identify types of NA
        sequences](#identify-types-of-na-sequences)
      - [Impute 0 - \<1 and short gaps](#impute-0---1-and-short-gaps)
      - [Impute missing final year
        values](#impute-missing-final-year-values)
      - [Impute problem cases](#impute-problem-cases)
      - [Mark imputed values](#mark-imputed-values)
      - [Spot check imputed series](#spot-check-imputed-series)
  - [Done, save](#done-save)

*Last updated on 24 April 2020*

UPDATE: there is a LOT of boutique imputation going on here. This might
take a while to update. The last time in 2020 took 12 hours. However,
this includes extra work by switching from the previous approach relying
more heavily on logistic growth models to more case-specific
imputations.

## Packages / functions

``` r
library("WDI")
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
library("ggplot2")
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
library("nlme")  # model cell phone growth curves
```

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

``` r
library("zoo")
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library("forecast")
```

    ## Registered S3 method overwritten by 'xts':
    ##   method     from
    ##   as.zoo.xts zoo

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## Registered S3 methods overwritten by 'forecast':
    ##   method             from    
    ##   fitted.fracdiff    fracdiff
    ##   residuals.fracdiff fracdiff

    ## 
    ## Attaching package: 'forecast'

    ## The following object is masked from 'package:nlme':
    ## 
    ##     getResponse

``` r
library("broom")
library("states")
library("readr")
```

    ## 
    ## Attaching package: 'readr'

    ## The following object is masked from 'package:states':
    ## 
    ##     parse_date

``` r
library("stringr")
library("tidyr")
library(purrr)

# Convert a WDI data frame to G&W statelist
# This will add a gwcode column. It also adds a row for Yugo/Serbia 2006,
# which is why this cannot be a function that creates a G&W column in the 
# existing data frame.
# @param x a data frame genereated by WDI::WDI()
# @returns a data frame like x, but with a gwcode column and extra row
wdi_to_gw <- function(x, iso2c = "iso2c", country = "country", year = "year") {
  
  # In case the ID columns don't match the WDI default, create them here. 
  # dplyr is easier to use when we can refer to these columns directly
  x$iso2c   <- x[[iso2c]]
  x$country <- x[[country]]
  x$year    <- x[[year]]
  
  # remove non-state entities, i.e. regions and aggregates
  notstate <- c("1A", "1W", "4E", "7E", "8S", "B8", "F1", "S1", "S2", "S3", "S4",
                "T2", "T3", "T4", "T5", "T6", "T7", "V1", "V2", "V3", "V4", "Z4",
                "Z7", "EU", "OE", "XC", "XD", "XE", "XF", "XG", "XH", "XI", "XJ",
                "XL", "XM", "XN", "XO", "XP", "XQ", "XT", "XU", "XY", "ZF", "ZG",
                "ZJ", "ZQ", "ZT")
  x <- x %>%
    dplyr::filter(!iso2c %in% notstate) 
  
  # first pass G&W country code coding
  x$gwcode <- suppressWarnings(countrycode::countrycode(x[["iso2c"]], "iso2c", "gwn"))

  # this misses some countries; first the fixed, non-year-varying, cases
  x <- x %>%
    mutate(
      gwcode = case_when(
        iso2c=="AD" ~ 232L,
        iso2c=="XK" ~ 347L,
        country=="Namibia" ~ 565L,
        iso2c=="VN" ~ 816L,  # countrycode uses 817, South Vietnam for this
        iso2c=="YE" ~ 678L,  # Yemen
        TRUE ~ gwcode
      )
    )

  # Fix Serbia/Yugoslavia
  # Right now all coded as 340, but in G&W 345 (Yugo) ends in 2006 and 
  # 340 (Serbia) starts
  serbia2006 <- x[x$gwcode==340 & x$year==2006 & !is.na(x$gwcode), ]
  yugo_idx <- x$gwcode==340 & x$year < 2007 & !is.na(x$gwcode)
  x$gwcode[yugo_idx]  <- 345
  x$iso2c[yugo_idx]   <- "YU"
  x$country[yugo_idx] <- "Yugoslavia/Serbia & Montenegro"
  x <- bind_rows(x, serbia2006) %>%
    arrange(gwcode, iso2c, country, year)
  
  x
}

#
#   Imputation-related functions
#   ____________________________
#
#   The imputation functions below are for parametric, curve-fitting growth
#   models. This .Rmd file has more custom ad-hoc imputation functions 
#   throughout.
#
#   I put these here at the top because the 2019 version of this script relied
#   heavily on logistic growth curve imputation, and because these imputation
#   functions are actually principled and maybe will be useful in the future
#   somewhere else (thus making it nice if they are easier to find...).
#
#   One side note (from 2020): I initially thought that for internet users
#   in percent it would make sense to run versions of these models that use
#   100 as Asym (the asymptote). That turned out to not work very well in that 
#   it produced a lot of estimation problems. Looking at series like that for 
#   the US for example, it also seems that this is not a valid assumption, and
#   that internet usage may plateau below 100%. 
#

growth_logistic <- function(x, year) {
  df <- tibble(x = x,
               # normalize year so that 1960 is 1; this helps with estimation
               time = year - 1959)
  fit <- tryCatch({
    nls(x ~ SSlogis(time, Asym, xmid, scal), data = df)
  }, error = function(e) {
    NULL
  })
  fit
}

growth_gompertz <- function(x, year) {
  df <- tibble(x = x,
               # normalize year so that 1960 is 1; this helps with estimation
               time = year - 1959)
  # gompertz doesn't work with 0 values
  df <- df[df$x > 0, ]
  fit <- tryCatch({
    nls(x ~ SSgompertz(time, Asym, b2, b3), data = df)
  }, error = function(e) {
    NULL
  })
  fit
}

# The functions below calculate MSE cost for each model.
# Importantly, because Gompertz does not work on 0 values in x, 
# the cost for both Gompertz and Logistic is only for the subset of x that 
# works with both models.

cost_gompertz <- function(x, year) {
  fit <- growth_gompertz(x, year)
  if (is.null(fit)) return(NA_real_)
  # calculate MSE
  mean(residuals(fit)^2)
}

cost_logistic <- function(x, year) {
  fit <- growth_logistic(x, year)
  if (is.null(fit)) return(NA_real_)
  # Gompertz does not work with 0 values, so only consider cost for > 0 x values
  resid <- residuals(fit)[which(x[!is.na(x)] > 0)]
  mean(resid^2)
}

impute_growth_logistic <- function(x, year) {
  fit  <- growth_logistic(x, year)
  xhat <- x
  if (is.null(fit)) return(xhat)
  xhat <- predict(fit, newdata = list(time = year - 1959))
  as.vector(xhat)
}

impute_growth_gompertz <- function(x, year) {
  fit  <- growth_gompertz(x, year)
  xhat <- x
  if (is.null(fit)) return(xhat)
  xhat <- predict(fit, newdata = list(time = year - 1959))
  as.vector(xhat)
}

# example, using internet users in South Sudan (unlagged year)
x <- c(NA, NA, NA, 3.829969245, 4.51615373, 5.5, 6.6797280510287, 
7.97742890716027, NA, NA, NA)
year <- 2010:2020
xhat <- impute_growth_logistic(x, year)
plot(year, x, xlab = "", ylab = "Internet users, %", ylim = c(0, max(xhat)*1.1),
     main = "Imputed internet users in South Sudan (red line)")
lines(year, xhat, col = "red")
```

![](clean-data_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## Explore possible indicators

*UPDATE: To trigger an update, delete the raw data input file or
manually run this chunk.*

``` r
if (!file.exists("input/ict-candidates.rds")) {
  wdi_cache <- WDIcache()
  ind <- c("IT.CEL", "IT.NET")
  vars <- ind %>% 
    map_df(function(x) {
      as_tibble(WDIsearch(x, field = "indicator", cache = wdi_cache))
    }) 
  
  all_vars <- WDI(country = "all", indicator = vars$indicator,
                  start = 1960, end = year(today()), extra = FALSE, cache = wdi_cache)
  
  write_rds(vars, "input/ict-candidates.rds")
  write_rds(all_vars, "input/ict-candidates-values.rds")
} else {
  vars     <- read_rds("input/ict-candidates.rds")
  all_vars <- read_rds("input/ict-candidates-values.rds")
}

sum_vars <- all_vars %>%
  select(-iso2c, -country, -year) %>%
  pivot_longer(everything(), names_to = "var", values_to = "value") %>%
  group_by(var) %>%
  summarize_all(list(N = ~n(), N_missing = ~sum(is.na(.)))) %>%
  ungroup() %>%
  mutate(Frac_missing = N_missing/N) %>%
  left_join(vars, ., by = c("indicator" = "var")) %>%
  filter(!is.na(N))

sum_vars %>%
  arrange(Frac_missing) %>%
  knitr::kable(digits = 2)
```

| indicator          | name                                                                         |     N | N\_missing | Frac\_missing |
| :----------------- | :--------------------------------------------------------------------------- | ----: | ---------: | ------------: |
| IT.CEL.SETS        | Mobile cellular subscriptions                                                | 16264 |       5325 |          0.33 |
| IT.CEL.SETS.P2     | Mobile cellular subscriptions (per 100 people)                               | 16264 |       5325 |          0.33 |
| IT.NET.USER.ZS     | Individuals using the Internet (% of population)                             | 16264 |       9979 |          0.61 |
| IT.NET.BBND        | Fixed broadband subscriptions                                                | 16264 |      12323 |          0.76 |
| IT.NET.BBND.P2     | Fixed broadband subscriptions (per 100 people)                               | 16264 |      12323 |          0.76 |
| IT.CEL.SETS.P3     | Mobile phone subscribers (per 1,000 people)                                  | 16264 |      13914 |          0.86 |
| IT.NET.SECR.P6     | Secure Internet servers (per 1 million people)                               | 16264 |      13949 |          0.86 |
| IT.CELL.MSUB.CD    | Mobile cellular monthly subscription (current US$)                           | 16264 |      14332 |          0.88 |
| IT.NET.SECR        | Secure Internet servers                                                      | 16264 |      14357 |          0.88 |
| IT.CELL.3MIN.CD.PK | Mobile cellular - price of 3-minute local call (peak rate - current US$)     | 16264 |      14384 |          0.88 |
| IT.CELL.PO.CONN.CD | Mobile cellular postpaid connection charge (current US$)                     | 16264 |      14401 |          0.89 |
| IT.CELL.MSUB.CN    | Mobile cellular monthly subscription (current LCU)                           | 16264 |      14489 |          0.89 |
| IT.CELL.3MIN.CN.PK | Mobile cellular - price of 3-minute local call (peak rate - current LCU)     | 16264 |      14539 |          0.89 |
| IT.CELL.PO.CONN.CN | Mobile cellular postpaid connection charge (current LCU)                     | 16264 |      14554 |          0.89 |
| IT.NET.USER        | Internet users                                                               | 16264 |      15179 |          0.93 |
| IT.NET.USER.P3     | Internet users (per 1,000 people)                                            | 16264 |      15179 |          0.93 |
| IT.CELL.3MIN.CD.OP | Mobile cellular - price of 3-minute local call (off-peak rate - current US$) | 16264 |      15333 |          0.94 |
| IT.CELL.3MIN.CN.OP | Mobile cellular - price of 3-minute local call (off-peak rate - current LCU) | 16264 |      15407 |          0.95 |
| IT.NET.BNDW.PC     | International Internet bandwidth (bits per person)                           | 16264 |      15509 |          0.95 |
| IT.NET.BNDW        | International Internet bandwidth (Mbps)                                      | 16264 |      15559 |          0.96 |
| IT.NET.BBND.P3     | Broadband subscribers (per 1,000 people)                                     | 16264 |      15633 |          0.96 |
| IT.CELL.PR.CONN.CD | Mobile cellular prepaid connection charge (current US$)                      | 16264 |      15978 |          0.98 |
| IT.CELL.PR.CONN.CN | Mobile cellular prepaid connection charge (current LCU)                      | 16264 |      15998 |          0.98 |
| IT.NET.SUB.CD      | Fixed broadband Internet monthly subscription (current US$)                  | 16264 |      16042 |          0.99 |
| IT.NET.SUB.CN      | Fixed broadband Internet monthly subscription (current LCU)                  | 16264 |      16060 |          0.99 |
| IT.NET.CONN.CD     | Fixed broadband Internet connection charge (current US$)                     | 16264 |      16078 |          0.99 |
| IT.NET.CONN.CN     | Fixed broadband Internet connection charge (current LCU)                     | 16264 |      16095 |          0.99 |

## Get raw data

Download the raw source data. Since this takes a while, this will only
raw if a copy of the source data is not present in the input folder.

*UPDATE: To trigger an update, delete the raw data input file or
manually run this chunk.*

``` r
if (!file.exists("input/ict.csv")) {
  raw <- WDI(indicator = c("IT.CEL.SETS.P2", "IT.NET.USER.ZS"), 
             country = "all", start = 1960, 
             end = year(today()), extra = FALSE)
  write_csv(raw, "input/ict.csv")
}
```

## Clean raw data

``` r
raw <- read_csv("input/ict.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   iso2c = col_character(),
    ##   country = col_character(),
    ##   year = col_double(),
    ##   IT.CEL.SETS.P2 = col_double(),
    ##   IT.NET.USER.ZS = col_double()
    ## )

``` r
# UPDATE: check this is still the case
# In the raw data all values for 2019 are missing; drop that year from the data
last_year <- filter(raw, year==max(year))
stopifnot(all(is.na(last_year[["IT.CEL.SETS.P2"]])))
raw <- filter(raw, year!=max(year))

# convert to G&W system
wdi <- raw %>%
  rename(cellphones_per100 = IT.CEL.SETS.P2,
         internet_users_pct = IT.NET.USER.ZS) %>%
  wdi_to_gw(.) %>%
  arrange(gwcode, year)

# Some minor countries don't have G&W codes
nogwcode <- wdi %>%
  filter(is.na(gwcode)) %>%
  group_by(iso2c, country) %>%
  count() 
write_csv(nogwcode, "output/missing-gwcode.csv")
knitr::kable(nogwcode)
```

| iso2c | country                        |  n |
| :---- | :----------------------------- | -: |
| AG    | Antigua and Barbuda            | 59 |
| AS    | American Samoa                 | 59 |
| AW    | Aruba                          | 59 |
| BM    | Bermuda                        | 59 |
| CW    | Curacao                        | 59 |
| DM    | Dominica                       | 59 |
| FM    | Micronesia, Fed. Sts.          | 59 |
| FO    | Faroe Islands                  | 59 |
| GD    | Grenada                        | 59 |
| GI    | Gibraltar                      | 59 |
| GL    | Greenland                      | 59 |
| GU    | Guam                           | 59 |
| HK    | Hong Kong SAR, China           | 59 |
| IM    | Isle of Man                    | 59 |
| JG    | Channel Islands                | 59 |
| KI    | Kiribati                       | 59 |
| KN    | St. Kitts and Nevis            | 59 |
| KY    | Cayman Islands                 | 59 |
| LC    | St. Lucia                      | 59 |
| LI    | Liechtenstein                  | 59 |
| MC    | Monaco                         | 59 |
| MF    | St. Martin (French part)       | 59 |
| MH    | Marshall Islands               | 59 |
| MO    | Macao SAR, China               | 59 |
| MP    | Northern Mariana Islands       | 59 |
| NC    | New Caledonia                  | 59 |
| NR    | Nauru                          | 59 |
| PF    | French Polynesia               | 59 |
| PR    | Puerto Rico                    | 59 |
| PS    | West Bank and Gaza             | 59 |
| PW    | Palau                          | 59 |
| SC    | Seychelles                     | 59 |
| SM    | San Marino                     | 59 |
| ST    | Sao Tome and Principe          | 59 |
| SX    | Sint Maarten (Dutch part)      | 59 |
| TC    | Turks and Caicos Islands       | 59 |
| TO    | Tonga                          | 59 |
| TV    | Tuvalu                         | 59 |
| VC    | St. Vincent and the Grenadines | 59 |
| VG    | British Virgin Islands         | 59 |
| VI    | Virgin Islands (U.S.)          | 59 |
| VU    | Vanuatu                        | 59 |
| WS    | Samoa                          | 59 |

``` r
# Take those out
wdi <- wdi %>%
  dplyr::filter(!is.na(gwcode))
```

Visualize series:

``` r
wdi %>%
  pivot_longer(cellphones_per100:internet_users_pct, names_to = "var") %>%
  filter(!is.na(value)) %>%
  ggplot(., aes(x = year, y = value)) +
  facet_wrap(~ var, scales = "free_y") +
  geom_line(aes(group = factor(iso2c)), alpha = 0.2) +
  scale_colour_discrete(guide = FALSE) +
  theme_minimal() +
  geom_smooth(se = FALSE)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](clean-data_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Lag data

*UPDATE: make sure the lagging is still correct*

Lag the data before we check for an if possible impute missing values
because the lagging will introduce missingness as well.

``` r
# I'm going to use this LAG variable below for other operations.
LAG <- 1
wdi$year <- wdi$year + LAG
range(wdi$year)
```

    ## [1] 1961 2019

### Normalize to G\&W statelist

``` r
# Add in missing cases from G&W state list and drop excess country-years not 
# in G&W list (left join)
statelist <- state_panel(1960,  # this cannot be min(year) because we lagged above
                         max(wdi$year), partial = "any")
wdi <- left_join(statelist, wdi, by = c("gwcode", "year")) %>%
  group_by(gwcode) %>%
  arrange(gwcode, year) %>%
  # iso2c and country are NA now for initial years, set those
  tidyr::fill(iso2c:country, .direction = "up")
```

## Handle missing values

The rest of this script deals with identifying and potentially imputing
missing values.

``` r
# The internet and cell phones did not take off as technologies until the 1990s.
# The raw data have missing and 0 values before 1990 (in the unlagged data),
# so we can set those to 0 before checking if any other years and countries
# are missing completely. 
#
# Check the distribution of values for both indicators by year.
by_year <- wdi %>%
  pivot_longer(cellphones_per100:internet_users_pct) %>%
  # convert the values in just an indicator if 0 or not
  mutate(value_is_0 = as.integer(value==0)) %>%
  # count by year what the distribution of 0/non-0 values is
  group_by(year, name) %>%
  summarize(value_is_0 = mean(value_is_0, na.rm = TRUE)) %>% 
  filter(!is.na(value_is_0)) 

# First (unlagged) year in which there are non-0 values
by_year %>%
  group_by(name) %>%
  arrange(name, year) %>%
  filter(value_is_0!=1 & lag(value_is_0)==1) %>%
  mutate(year = year - LAG)
```

    ## # A tibble: 2 x 3
    ## # Groups:   name [2]
    ##    year name               value_is_0
    ##   <dbl> <chr>                   <dbl>
    ## 1  1980 cellphones_per100       0.993
    ## 2  1990 internet_users_pct      0.883

``` r
# For cellphones, first non-0 (unlagged) year is 1980
# For internet users, it is 1990
by_year %>%
  ungroup() %>%
  mutate(year = year - LAG) %>%
  ggplot(aes(x = year, y = value_is_0, color = name)) +
  geom_line() +
  labs(x = "Unlagged, original year")
```

![](clean-data_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# Keep track of the original data frame so we can later ID imputed values
wdi_orig <- wdi %>%
  select(-country, -iso2c) %>%
  setNames(c("gwcode", "year", paste0("orig_", names(.)[3:ncol(.)])))

# We can thus set values before those years to 0
wdi$cellphones_per100[ (wdi$year - LAG) < 1980] <- 0
wdi$internet_users_pct[ (wdi$year - LAG) < 1990] <- 0

# Are any years completely missing?
missing_year <- wdi %>%
  group_by(year) %>%
  summarize(n = n(), 
            missing = sum(is.na(cellphones_per100) | is.na(internet_users_pct))) %>%
  filter(n==missing) 
write_csv(missing_year, "output/missing-all-year.csv")
missing_year %>%
  knitr::kable()
```

| year | n | missing |
| ---: | -: | ------: |

``` r
# No missing years
stopifnot(nrow(missing_year) == 0)

# GDR (ends 1990), South Yemen (ends 1990), and Czechoslovakia (ends 1992) have
# missing values for cell phones in the 80s and internet in the 90s (CZ)
# I'm ok assuming those are 0. 
wdi$cellphones_per100[wdi$gwcode %in% c(265, 315, 680)] <- 0
wdi$internet_users_pct[wdi$gwcode %in% c(265, 315)] <- 0

# Are any countries completely missing?
# We need to account for the 0 values we set above, as well.
missing_country <- wdi %>%
  group_by(gwcode) %>%
  mutate(
    n = n(),
    # Mark years for which cell and internet values should have been observed
    cell_obs = (year - LAG) >= 1980,
    inet_obs = (year - LAG) >= 1990,
    # Mark problematic missing values (as opposed to missing values prior to 
    # non-0 years)
    cell_na = cell_obs & is.na(cellphones_per100),
    inet_na = inet_obs & is.na(internet_users_pct)
  ) %>%
  summarize(
    # number of potential cell observed values
    cell_n = sum(cell_obs),
    # number of those values that are missing
    missing_cell = sum(cell_na),
    missing_cell_years = ifelse(sum(cell_na)==0, "", 
                                paste0(range(year[cell_na]), collapse = " - ")),
    # same for internet
    inet_n = sum(inet_obs),
    missing_inet = sum(inet_na),
    missing_inet_years = ifelse(sum(inet_na)==0, "", 
                                paste0(range(year[inet_na]), collapse = " - "))
  ) %>%
  filter(
    # indicate whether either cell phones or internet series are completely
    # missing
    (missing_cell==cell_n & cell_n > 0) | (missing_inet==inet_n & inet_n > 0)
  ) %>%
  mutate(country = country_names(gwcode, shorten = TRUE)) %>%
  select(gwcode, country, everything())
write_csv(missing_country, "output/missing-all-country.csv")
missing_country %>%
  knitr::kable()
```

| gwcode | country               | cell\_n | missing\_cell | missing\_cell\_years | inet\_n | missing\_inet | missing\_inet\_years |
| -----: | :-------------------- | ------: | ------------: | :------------------- | ------: | ------------: | :------------------- |
|     54 | Dominica              |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|     55 | Grenada               |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|     56 | Saint Lucia           |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|     57 | Saint Vincent         |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|     58 | Antigua & Barbuda     |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|     60 | Saint Kitts and Nevis |      37 |            37 | 1983 - 2019          |      29 |            29 | 1991 - 2019          |
|    221 | Monaco                |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|    223 | Liechtenstein         |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|    331 | San Marino            |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|    396 | Abkhazia              |      12 |            12 | 2008 - 2019          |      12 |            12 | 2008 - 2019          |
|    397 | South Ossetia         |      12 |            12 | 2008 - 2019          |      12 |            12 | 2008 - 2019          |
|    403 | Sao Tome and Principe |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|    591 | Seychelles            |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|    713 | Taiwan                |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|    935 | Vanuatu               |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|    970 | Kiribati              |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|    971 | Nauru                 |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|    972 | Tonga                 |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|    973 | Tuvalu                |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |
|    983 | Marshall Islands      |      34 |            34 | 1986 - 2019          |      29 |            29 | 1991 - 2019          |
|    986 | Palau                 |      26 |            26 | 1994 - 2019          |      26 |            26 | 1994 - 2019          |
|    987 | Micronesia            |      34 |            34 | 1986 - 2019          |      29 |            29 | 1991 - 2019          |
|    990 | Samoa/Western Samoa   |      39 |            39 | 1981 - 2019          |      29 |            29 | 1991 - 2019          |

``` r
# Take out countries missing all values
wdi <- wdi %>% 
  dplyr::filter(!gwcode %in% missing_country[["gwcode"]])
```

### Identify types of NA sequences

``` r
# I want to pull out NA gaps that have non-NA values on both sides; specifically
# both the gap and the surrounding non-NA values
# Version of rle for which NA==NA is true
na_rle <- function(x) {
  if (!is.vector(x) && !is.list(x)) 
    stop("'x' must be a vector of an atomic type")
  n <- length(x)
  if (n == 0L) 
    return(structure(list(lengths = integer(), values = x), 
                     class = "rle"))
  # the only change is to this:
  # y <- x[-1L] != x[-n]
  y <- !na_equal(x[-1L], x[-n])
  i <- c(which(y | is.na(y)), n)
  structure(list(lengths = diff(c(0L, i)), values = x[i]), 
            class = "rle")
}

# version of == that for which NA==NA is true, NA==1 is false
# isTRUE(na_equal(NA, NA))
# isFALSE(na_equal(NA, 2))
# isTRUE(na_equal(2, 2))
na_equal <- function(a, b) {
  stopifnot(length(a)==length(b))
  z = logical(length(a))
  for (i in seq_along(a)) {
    if (is.na(a[i]) & is.na(b[i])) {
      z[i] <- TRUE
      next
    }
    if (xor(is.na(a[i]), is.na(b[i]))) {
      z[i] <- FALSE
      next 
    }
    z[i] <- (a[i]==b[i])
  }
  z
}

extract_na_gaps <- function(x) {
  na_rle <- na_rle(is.na(x))
  nn <- length(na_rle$lengths)
  na_rle$start <- cumsum(c(1, na_rle$lengths[-nn])) 
  na_rle$start[-1] <- na_rle$start[-1] - 1
  na_rle$end  <- cumsum(na_rle$lengths)
  na_rle$end[-nn] <- na_rle$end[-nn] + 1
  
  gaps <- list()
  for (i in seq_along(na_rle$values)) {
    if (na_rle$values[i]==TRUE) {
      gap_i <- x[na_rle$start[i]:na_rle$end[i]]
      gaps <- c(gaps, list(gap_i))
    }
  }
  gaps
}

x <- c(NA, NA, 1, 2, NA, 3, NA, NA, 5, NA, NA, 6)
extract_na_gaps(x)
```

    ## [[1]]
    ## [1] NA NA  1
    ## 
    ## [[2]]
    ## [1]  2 NA  3
    ## 
    ## [[3]]
    ## [1]  3 NA NA  5
    ## 
    ## [[4]]
    ## [1]  5 NA NA  6

``` r
gap_wrapper <- function(x, y) {
  gaps <- extract_na_gaps(x)
  gaps <- lapply(gaps, function(x) {
    tibble(yleft = x[1],
           yright = tail(x, 1),
           n = length(x))
  }) %>%
    bind_rows()
  gaps <- crossing(y, gaps)
  gaps
}

inet_gaps <- wdi %>%
  group_by(gwcode) %>% 
  group_map(~gap_wrapper(.x$internet_users_pct, .y)) %>%
  bind_rows() %>%
  mutate(variable = "internet_users_pct")

cell_gaps <- wdi %>%
  group_by(gwcode) %>% 
  group_map(~gap_wrapper(.x$cellphones_per100, .y)) %>%
  bind_rows() %>%
  mutate(variable = "cellphones_per100")

all_gaps <- bind_rows(inet_gaps, cell_gaps)

# if the gap is at the head or tail of a series, yleft or yright will be NA;
# filter out those
gaps <- all_gaps %>%
  filter(!is.na(yleft) & !is.na(yright))
gaps
```

    ## # A tibble: 167 x 5
    ##    gwcode   yleft    yright     n variable          
    ##     <dbl>   <dbl>     <dbl> <int> <chr>             
    ##  1     31 0       0.960         6 internet_users_pct
    ##  2     40 0       0.0000917     6 internet_users_pct
    ##  3     41 0       0.00748       7 internet_users_pct
    ##  4     41 0.00748 0.0240        3 internet_users_pct
    ##  5     42 0       0.0172        6 internet_users_pct
    ##  6     51 0       0.0368        5 internet_users_pct
    ##  7     52 0       0.158         6 internet_users_pct
    ##  8     53 0       0.00775       6 internet_users_pct
    ##  9     80 0       0.0454        6 internet_users_pct
    ## 10     80 5.68    9.8           3 internet_users_pct
    ## # … with 157 more rows

``` r
# How many gaps are 0 to <1?
idx_0_to_lt1 <- (gaps$yleft==0) & (gaps$yright < 1)
sum(idx_0_to_lt1) / nrow(gaps)
```

    ## [1] 0.8443114

``` r
# 84% of gaps are 0 to <1; i think these are safe to set to 0

# How many of the other gaps span only 1 value? 
# (n includes yleft and yright, so n = 3 is a single missing value)
sum(!idx_0_to_lt1 & gaps$n==3) / nrow(gaps)
```

    ## [1] 0.07784431

``` r
# 8%; I'd say these are safe to linear impute

# How many does that leave?
sum(!idx_0_to_lt1 & !gaps$n==3)
```

    ## [1] 13

``` r
# 13 that require more attention
```

### Impute 0 - \<1 and short gaps

``` r
imputer <- function(x) {
  xhat <- x
  n <- length(x)
  not_na_idx <- which(!is.na(x))
  # first non-missing value on the left, or NA
  xleft  <- NA
  # first non-missing value on the right, or NA
  xright <- NA
  for (i in seq_along(x)) {
    # determine xright, if possible
    if (!any(i < not_na_idx)) {
      xright <- NA
    } else {
      xright <- x[not_na_idx[which(i < not_na_idx)[1]]]
    }

    # determine xleft
    if (i > 1 & !is.na(x[i])) {
      xleft <- x[i]
    }
    
    # try to impute
    if (is.na(x[i])) {
      # case 1: left and right non-NA values are (0 or NA) and <1
      if ( (isTRUE(xleft==0) | isTRUE(is.na(xleft))) & isTRUE(xright < 1)) {
        xhat[i] <- 0
      }
      # case 2: single missing value with non-missing left/right values
      # exclude first and last x, so the indexing that comes next doesn't error
      if (i > 1 & i < n) {
        if (!any(is.na(x[i-1]), is.na(x[i+1]))) {
          xhat[i] <- (xleft + xright) / 2
        }
      }
    }
  }
  xhat
}

x       <- c(NA, 0, 0, NA, NA, 0.5, 1, NA, 3, NA, NA, 5, NA)
correct <- c(0, 0, 0, 0, 0, 0.5, 1, 2, 3, NA, NA, 5, NA)
# the first, second-to-last, and last NA sequencess should not be imputed
stopifnot(all.equal(correct, imputer(x)))

# Apply the imputer
wdi <-  wdi %>%
  group_by(gwcode) %>%
  arrange(gwcode, year) %>%
  mutate(cellphones_per100 = imputer(cellphones_per100),
         internet_users_pct = imputer(internet_users_pct))
```

### Impute missing final year values

Many countries are only missing values for the last year in the data.
Pull those out and decide on an imputation strategy.

``` r
inet <- wdi %>%
  select(gwcode, year, country, internet_users_pct) %>%
  group_by(gwcode) %>%
  filter(is.na(internet_users_pct[year==max(year)]),
         sum(is.na(internet_users_pct))==1)

# Keep this for later so we can visualize the imputed values
inet_tail_imputed <- unique(inet$gwcode)

# Check growth rates
inet <- inet %>%
  mutate(growth_factor = internet_users_pct / lag(internet_users_pct)) %>%
  slice((n() - 5):(n()-1)) %>%
  mutate(avg_growth_factor = mean(growth_factor))

# Use the average growth rate over last 5 periods to impute
imputer_recent_growth <- function(x, upper_limit = Inf) {
  n <- length(x)
  if (!is.na(tail(x, 1)) | n < 7) return(x)   
  # do this test separately because if the series is too short this will cause 
  # a subscript error
  if (anyNA(x[(n-6):(n-1)])) return(x)
  gf <- x / lag(x)
  gf <- gf[-n]
  agf <- mean(tail(gf, 5))
  x[n] <- x[n-1] * agf
  if (x[n] > upper_limit) x[n] <- upper_limit
  x
}

# Make sure it works correctly
# wdi %>% filter(gwcode==51) %>% pull(internet_users_pct) -> x
x <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.036817104, 0.109487986, 
0.591145842, 0.797797441, 1.978726318, 2.355688556, 3.11577802729967, 
3.8630203874764, 6.1, 7.8, 10, 12.8, 16.4, 21.1, 23.6, 24.3, 
27.67, 37.438613414848, 33.79, 37.1, 40.4027351046672, 42.2212211907441, 
44.36685637, 55.0720670507751, NA)
plot(1962:2019, imputer_recent_growth(x), xlab = "", ylab = "Internet users, %",
     main = "Red = orig series; black points = imputed series")
lines(1962:2019, x, col = "red")
```

![](clean-data_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
wdi <- wdi %>%
  mutate(internet_users_pct = imputer_recent_growth(internet_users_pct),
         cellphones_per100 = imputer_recent_growth(cellphones_per100))
```

### Impute problem cases

``` r
# Serbia is missing 2006 values. We actually have these from 345 so drop them 
# in
serbia2006 <- wdi$gwcode==340 & wdi$year==2006
yugo2006   <- wdi$gwcode==345 & wdi$year==2006
wdi$cellphones_per100[serbia2006] <- wdi$cellphones_per100[yugo2006]
wdi$internet_users_pct[serbia2006] <- wdi$internet_users_pct[yugo2006]

inet_gaps <- wdi %>%
  group_by(gwcode) %>% 
  group_map(~gap_wrapper(.x$internet_users_pct, .y)) %>%
  bind_rows() %>%
  mutate(variable = "internet_users_pct")

cell_gaps <- wdi %>%
  group_by(gwcode) %>% 
  group_map(~gap_wrapper(.x$cellphones_per100, .y)) %>%
  bind_rows() %>%
  mutate(variable = "cellphones_per100")

all_gaps <- bind_rows(inet_gaps, cell_gaps)

problems <- all_gaps %>%
  mutate(flag = 1)

wdi %>%
  pivot_longer(cellphones_per100:internet_users_pct, names_to = "variable") %>%
  left_join(problems) %>%
  filter(flag==1) %>%
  ggplot(aes(x = year, y = value, group = interaction(gwcode, variable))) +
  facet_wrap(~ gwcode + variable, scales = "free_x") + 
  geom_line() +
  theme_minimal()
```

    ## Joining, by = c("gwcode", "variable")

    ## Warning: Removed 34 rows containing missing values (geom_path).

<img src="clean-data_files/figure-gfm/unnamed-chunk-8-1.png" height="8" />

#### Linear impute gaps with \< 9 NAs and values on either side

``` r
# Many of these are gaps with complete series on either side and length >1 but 
# less than 5 or so. Linear impute these.
linear_impute_countries <- all_gaps %>% 
  filter(!is.na(yleft), !is.na(yright), n < 10) 

linear_impute <- function(x) {
  n <- length(x)
  indx <- (1:n)[!is.na(x)]
  xhat <- stats::approx(indx, x[indx], 1:n, rule = 1)$y
  xhat
}

# test it out with 232
x <- wdi$internet_users_pct[wdi$gwcode==232]
x[c(1:2, length(x))] <- NA  # this should not be imputed
xhat <- linear_impute(x)
stopifnot(all(is.na(x[c(1:2, length(x))])))
plot(x, xlab = "", ylab = "")
lines(xhat, col = "red")
```

![](clean-data_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
# Apply the linear imputation
wdi <- wdi %>%
  group_by(gwcode) %>%
  arrange(gwcode, year)
for (i in 1:nrow(linear_impute_countries)) {
  gwc <- linear_impute_countries$gwcode[i]
  var <- linear_impute_countries$variable[i]
  
  # subscript with `[[` so we get a vector, not a 1-column tibble
  wdi[wdi$gwcode==gwc, ][[var]] <- linear_impute(wdi[wdi$gwcode==gwc, ][[var]])
}
```

#### Country-specific imputations

UPDATE: next are all country-specific imputations. This might all need
manual checking.

``` r
# East Timor cellphones; missing first 2 values for 2002-2003
x <- wdi$cellphones_per100[wdi$gwcode==860]
stopifnot(sum(is.na(x))==2)
# the growth factor year on year for 2004 and 2005 was ~1.25; project that back
x[2] <- x[3]/1.25
x[1] <- x[2]/1.25
wdi$cellphones_per100[wdi$gwcode==860] <- x

# Internet users in North Korea; it's 0 until 2012 (unlagged) and NA after that
# this place, https://www.internetworldstats.com/asia/kp.htm, sourced from 
# a UN office, claims 14,000 for 2016 and 2017. That's ~0.056 percent. So, keep
# it at 0.
x <- wdi$internet_users_pct[wdi$gwcode==731]
x[is.na(x)] <- 0
wdi$internet_users_pct[wdi$gwcode==731] <- x

# South Sudan, internet users
# Try logistic growth model
x <- wdi$internet_users_pct[wdi$gwcode==626]
year <- wdi$year[wdi$gwcode==626]
xhat <- impute_growth_logistic(x, year)
plot(year, x, xlab = "", ylab = "%", main = "Internet users, South Sudan",
     ylim = c(0, max(xhat)*1.1))
lines(year, xhat, col = "red")
```

![](clean-data_files/figure-gfm/country-specific-imputations-1.png)<!-- -->

``` r
# looks good
wdi$internet_users_pct[wdi$gwcode==626] <- xhat

# 
#   Cell phone and internet users in Yugoslavia
#   ______________
#
#   This one is special because we have post-2006 series for Serbia and Montenegro
#
vars <- c("gwcode", "year", "cellphones_per100", "internet_users_pct")
yu <- wdi[wdi$gwcode==345, vars]
rs <- wdi[wdi$gwcode==340, vars]
me <- wdi[wdi$gwcode==341, vars]

all <- bind_rows(yu, rs, me) %>% pivot_longer(-c(gwcode, year))
all %>% ggplot(aes(x = year, y = value, color = factor(gwcode))) + 
  facet_wrap(~ name, ncol = 1, scales = "free_y") + geom_line()
```

![](clean-data_files/figure-gfm/country-specific-imputations-2.png)<!-- -->

``` r
# Ok. Since Serbia has ~10 times the population of Montenegro, I'm goning to 
# use Serbia's cellphones series as a target; internet are both similar. 
# 
#   Start with internet 
#   ___________
#
year <- c(yu$year, rs$year[rs$year > max(yu$year)])
# Make sure 2006 is not double
stopifnot(sum(year==2006)==1)

x <- c(yu$internet_users_pct, rs$internet_users_pct[rs$year > max(yu$year)])

cost_logistic(x, year)
```

    ## [1] 4.014495

``` r
cost_gompertz(x, year)
```

    ## [1] 3.651581

``` r
# Looks like Gompertz has better fit; what do they look like visually?

x_log <- impute_growth_logistic(x, year)
x_gom <- impute_growth_gompertz(x, year)

plot(year, x, xlab = "", ylab = "%", main = "Internet in Yugoslavia, red = logistic, blue = gompertz")
lines(year, x_log, col = "red")
lines(year, x_gom, col = "blue")
```

![](clean-data_files/figure-gfm/country-specific-imputations-3.png)<!-- -->

``` r
# Ok, let's do Gompertz. It seems to fit better with the 0's for 1989 and before.
x <- wdi$internet_users_pct[wdi$gwcode==345]
x[is.na(x)] <- x_gom[1:length(x)][is.na(x)]
wdi$internet_users_pct[wdi$gwcode==345] <- x

#
#   Cellphones
#   _________

x <- c(yu$cellphones_per100, rs$cellphones_per100[rs$year > max(yu$year)])

cost_logistic(x, year)
```

    ## [1] 36.03937

``` r
cost_gompertz(x, year)
```

    ## [1] NA

``` r
# Logistic it is (Gompertz doesn't estimate)

xhat <- impute_growth_logistic(x, year)

plot(year, x, xlab = "", ylab = "%", main = "Cell phones in Yugoslavia, red = logistic")
lines(year, xhat, col = "red")
```

![](clean-data_files/figure-gfm/country-specific-imputations-4.png)<!-- -->

``` r
# Replace NA values
x <- wdi$cellphones_per100[wdi$gwcode==345]
x[is.na(x)] <- xhat[1:length(x)][is.na(x)]
wdi$cellphones_per100[wdi$gwcode==345] <- x

#
#   Cell phone and internet users in Kosovo
#   ______________________________________
# 
#   This is tricky because there very few data points. I'm going to guess 
#   some logistic growth based on values from Serbia and Albania. In 1999
#   there was also conflict, so I'm going to use reference values for those
#   years too.
#

# Internet usage
x <- wdi$internet_users_pct[wdi$gwcode==347]
year <- wdi$year[wdi$gwcode==347]

# Starting values for 1999 and 2008 (unlagged) in Serbia and Albania
wdi$internet_users_pct[wdi$gwcode==345 & wdi$year==(1999 + LAG)]
```

    ## [1] 11.54035

``` r
wdi$internet_users_pct[wdi$gwcode==340 & wdi$year==(2008 + LAG)]
```

    ## [1] 35.6

``` r
wdi$internet_users_pct[wdi$gwcode==339 & wdi$year %in% (c(1999, 2008)+LAG)]
```

    ## [1]  0.08143704 23.86000000

``` r
#             1999    2008
# Serbia/YU   11.5    35.6
# Albania      0.1    23.9
#
# Extend the series so we can drop some reference values in
# adjust this if LAG changes
stopifnot(LAG==1)
year <- 1999:2018+LAG
x <- c(rep(NA, length(year) - length(x)), x)
# There are some news articles suggesting a boom in ICT in Kosovo after 1999
# So low-ball the 1999 and high-ball 2008 value
x[year==(1999+LAG)] <- 1  
x[year==(2008+LAG)] <- 35 

# The estimated asymptote is ~90, this is reasonable. 
growth_logistic(x, year)
```

    ## Nonlinear regression model
    ##   model: x ~ SSlogis(time, Asym, xmid, scal)
    ##    data: df
    ##   Asym   xmid   scal 
    ## 91.580 51.382  2.802 
    ##  residual sum-of-squares: 9.218
    ## 
    ## Number of iterations to convergence: 0 
    ## Achieved convergence tolerance: 9.006e-06

``` r
xhat <- impute_growth_logistic(x, year)
plot(year, x, main = "Kosovo internet usage", ylab = "%", xlab = "")
lines(year, xhat, col = "red")
```

![](clean-data_files/figure-gfm/country-specific-imputations-5.png)<!-- -->

``` r
orig_year <- wdi$year[wdi$gwcode==347]
wdi$internet_users_pct[wdi$gwcode==347] <- xhat[year %in% orig_year]

# Cell phones
x <- wdi$cellphones_per100[wdi$gwcode==347]
year <- wdi$year[wdi$gwcode==347]

# Starting values for 1999 and 2018 (unlagged) in Serbia and Albania
wdi$cellphones_per100[wdi$gwcode==345 & wdi$year==(1999 + LAG)]
```

    ## [1] 1.707074

``` r
wdi$cellphones_per100[wdi$gwcode==340 & wdi$year==(2018 + LAG)]
```

    ## [1] 95.78099

``` r
wdi$cellphones_per100[wdi$gwcode==339 & wdi$year %in% (c(1999, 2018)+LAG)]
```

    ## [1]  0.3525158 94.1769983

``` r
#             1999    2018
# Serbia/YU    1.7    95.8
# Albania      0.4    94.2
#
# Extend the series so we can drop some reference values in
# adjust this if LAG changes
stopifnot(LAG==1)
year <- 1999:2018+LAG
x <- c(rep(NA, length(year) - length(x)), x)
# There are some news articles suggesting a boom in ICT in Kosovo after 1999
# So low-ball the 1999 and high-ball 2008 value
x[year==(1999+LAG)] <- 1  
x[year==(2018+LAG)] <- 95

# The estimated asymptote is ~90, this is reasonable. 
growth_logistic(x, year)
```

    ## Nonlinear regression model
    ##   model: x ~ SSlogis(time, Asym, xmid, scal)
    ##    data: df
    ##    Asym    xmid    scal 
    ## 200.844  60.687   6.002 
    ##  residual sum-of-squares: 139.3
    ## 
    ## Number of iterations to convergence: 3 
    ## Achieved convergence tolerance: 6.567e-06

``` r
xhat <- impute_growth_logistic(x, year)
plot(year, x, main = "Kosovo cell phones", ylab = "per 100", xlab = "")
lines(year, xhat, col = "red")
```

![](clean-data_files/figure-gfm/country-specific-imputations-6.png)<!-- -->

``` r
# This isn't working very well. 
df <- tibble(x = x,
             # normalize year so that 1960 is 1; this helps with estimation
             time = year - 1959)
fit <- SSlogis(df$time, Asym = 110, xmid = 52, scal = 2)
plot(year, x, xlab = "", ylab = "per 100", main = "Kosovo cell phones", 
     ylim = c(0, max(fit)*1.1))
lines(year, fit, col = "red")
```

![](clean-data_files/figure-gfm/country-specific-imputations-7.png)<!-- -->

``` r
# this kind of works but we get a sudden jump from the first imputed value
# ...just linear impute with the assumed ending value
xhat <- linear_impute(x)
plot(year, x, xlab = "", ylab = "per 100", main = "Kosovo cell phones, linear impute", 
     ylim = c(0, max(fit)*1.1))
lines(year, xhat, col = "red")
```

![](clean-data_files/figure-gfm/country-specific-imputations-8.png)<!-- -->

``` r
n <- length(wdi$cellphones_per100[wdi$gwcode==347])
wdi$cellphones_per100[wdi$gwcode==347] <- tail(xhat, n)
```

### Mark imputed values

``` r
# Mark imputed values 
# This is some helper code below to set up this big blog of text
# cat(paste0(names(wdi), "_imputed = as.integer(!is.na(", names(wdi), ") &\n is.na(orig_", names(wdi), "))"), sep = ",\n")
# 
# There's probably a way to do this with mutate_at but I'm not going to mess 
# around with quosures
wdi <- left_join(wdi, wdi_orig, by = c("gwcode", "year")) %>%
  mutate(
    cellphones_per100_imputed = as.integer(!is.na(cellphones_per100) &
                                             is.na(orig_cellphones_per100)),
    internet_users_pct_imputed = as.integer(!is.na(internet_users_pct) &
                                              is.na(orig_internet_users_pct))
  ) %>%
  select(-starts_with("orig_"))
```

### Spot check imputed series

``` r
check <- c(110, 345, 347, 626, 40)
check_data <- wdi %>%
  filter(gwcode %in% check) %>%
  pivot_longer(-c(gwcode, year, iso2c, country)) %>%
  mutate(value_type = ifelse(stringr::str_detect(name, "imputed"), "imputed", "value"),
         name = str_remove(name, "_imputed")) %>%
  pivot_wider(names_from = value_type, values_from = value)
ggplot(check_data, aes(x = year, y = value)) +
  facet_grid(factor(gwcode) ~ name) +
  geom_line() +
  geom_point(data = check_data %>% filter(imputed==TRUE), color = "red")
```

![](clean-data_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Done, save

``` r
wdi <- wdi %>% 
  ungroup() %>%
  select(gwcode, year, everything(), -iso2c, -country) 
write_csv(wdi, path = "output/wdi-ict.csv")
```
