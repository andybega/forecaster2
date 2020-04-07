V-Dem
================

``` r
library(readr)
library(states)
```

    ## 
    ## Attaching package: 'states'

    ## The following object is masked from 'package:readr':
    ## 
    ##     parse_date

``` r
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
raw = read_csv("input/v10/V-Dem-CY-Core-v10.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   country_name = col_character(),
    ##   country_text_id = col_character(),
    ##   historical_date = col_date(format = ""),
    ##   histname = col_character(),
    ##   gapstart1 = col_logical(),
    ##   gapstart2 = col_logical(),
    ##   gapstart3 = col_logical(),
    ##   gapend1 = col_logical(),
    ##   gapend2 = col_logical(),
    ##   gapend3 = col_logical()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 8330 parsing failures.
    ##  row       col           expected actual                              file
    ## 2075 gapstart1 1/0/T/F/TRUE/FALSE   1851 'input/v10/V-Dem-CY-Core-v10.csv'
    ## 2075 gapend1   1/0/T/F/TRUE/FALSE   1917 'input/v10/V-Dem-CY-Core-v10.csv'
    ## 2076 gapstart1 1/0/T/F/TRUE/FALSE   1851 'input/v10/V-Dem-CY-Core-v10.csv'
    ## 2076 gapend1   1/0/T/F/TRUE/FALSE   1917 'input/v10/V-Dem-CY-Core-v10.csv'
    ## 2077 gapstart1 1/0/T/F/TRUE/FALSE   1851 'input/v10/V-Dem-CY-Core-v10.csv'
    ## .... ......... .................. ...... .................................
    ## See problems(...) for more details.

``` r
v2x = raw %>%
  select(country_name, COWcode, year, starts_with("v2x_")) %>%
  select(-ends_with("codehigh"), -ends_with("codelow"), -ends_with("sd"),
         -ends_with("osp")) %>%
  # take out the discrete regime variable, it will get treated as integer
  # and i don't want to deal with dummying it out
  select(-starts_with("v2x_regime")) %>%
  # G&W doesn't start until 1816, so take out earlier
  filter(year > 1815)

# Change from COW codes to G&W codes
v2x = v2x %>%
  rename(gwcode = COWcode) %>%
  mutate(gwcode = case_when(
    gwcode == 255 ~ 260,
    gwcode == 679 ~ 678,
    TRUE ~ gwcode))

# How many missing values are there for each variable?
sapply(v2x, function(x) sum(is.na(x))) %>%
  tibble::enframe(name = "variable", value = "missing") %>%
  arrange(desc(missing)) %>%
  knitr::kable()
```

| variable            | missing |
| :------------------ | ------: |
| v2x\_delibdem       |    6664 |
| v2x\_egaldem        |    6664 |
| v2x\_divparctrl     |    6657 |
| v2x\_accountability |    6571 |
| v2x\_veracc         |    6571 |
| v2x\_diagacc        |    6571 |
| v2x\_horacc         |    6571 |
| v2x\_egal           |    6569 |
| v2x\_gender         |    4450 |
| v2x\_genpp          |    4213 |
| v2x\_libdem         |    1958 |
| v2x\_neopat         |    1558 |
| v2x\_partipdem      |    1533 |
| v2x\_liberal        |    1417 |
| v2x\_jucon          |    1349 |
| v2x\_polyarchy      |    1203 |
| v2x\_api            |    1203 |
| v2x\_mpi            |    1203 |
| v2x\_EDcomp\_thick  |    1065 |
| v2x\_corr           |    1059 |
| v2x\_ex\_direlect   |     945 |
| v2x\_partip         |     944 |
| v2x\_execorr        |     793 |
| v2x\_pubcorr        |     669 |
| v2x\_elecoff        |     646 |
| v2x\_frassoc\_thick |     641 |
| gwcode              |     632 |
| v2x\_freexp\_altinf |     561 |
| v2x\_gencs          |     556 |
| v2x\_cspart         |     554 |
| v2x\_freexp         |     515 |
| v2x\_ex\_confidence |     462 |
| v2x\_civlib         |     416 |
| v2x\_clpol          |     407 |
| v2x\_rule           |     396 |
| v2x\_hosabort       |     343 |
| v2x\_hosinter       |     343 |
| v2x\_legabort       |     343 |
| v2x\_ex\_hereditary |     324 |
| v2x\_ex\_military   |     324 |
| v2x\_ex\_party      |     324 |
| v2x\_suffr          |     256 |
| v2x\_clphy          |     235 |
| v2x\_clpriv         |     232 |
| v2x\_gencl          |     232 |
| v2x\_feduni         |      45 |
| country\_name       |       0 |
| year                |       0 |
| v2x\_elecreg        |       0 |

``` r
#
#   Add variable transformations ----
#   ___________________________ 

# first we need to identify country-spells, to make sure gaps don't accidentally
# cross over into what are supposed to be year to year changes
v2x = v2x %>%
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(spell_id = id_date_sequence(year))

v2x_feats = v2x %>%
  group_by(country_name) %>%
  mutate_at(vars(starts_with("v2x_")), list(diff = ~c(0, diff(.))))

write_csv(v2x, "output/v-dem.csv")
```
