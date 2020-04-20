Ethnic Power Relations (EPR)
================

*Last updated on 20 April 2020*

Source: <https://icr.ethz.ch/data/epr/core/>

License: not clear

Citation: Vogt, Manuel, Nils-Christian Bormann, Seraina Rüegger,
Lars-Erik Cederman, Philipp Hunziker, and Luc Girardin. 2015.
“Integrating Data on Ethnicity, Geography, and Conflict: The Ethnic
Power Relations Data Set Family.” Journal of Conflict Resolution 59(7):
1327–42.

To update:

1.  Download and get EPR data from the URL above.

2.  Run the `clean-data.Rmd` script (interactively\!) and adjust as
    needed base on new facts on the ground (search for “UPDATE”).

3.  The main output is “output/epr.csv”.

Changes to the data:

  - the data are lagged by 2 years in order to have values for 2019
  - the resulting initial 2 years of missing values are carry-back
    imputed, i.e. taken to be the first observed value for a country;
    otherwise there are no imputations
  - some small states are not covered by the data
  - several variable transformations are added: binary indicators for
    whether there have been shifts in the excluded or inpower groups;
    and year to year changes in the excluded/inpower variables

## Usage

``` r
library(readr)
library(dplyr)

epr <- read_csv("output/epr.csv")

glimpse(epr)
```

    ## Rows: 10,488
    ## Columns: 16
    ## $ gwcode                     <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    ## $ year                       <dbl> 1946, 1947, 1948, 1949, 1950, 1951, 1952, …
    ## $ groups                     <dbl> 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, …
    ## $ elf                        <dbl> 0.5098565, 0.5098565, 0.5098565, 0.5098565…
    ## $ excluded_groups_count      <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    ## $ excluded_groups_pop        <dbl> 0.1318, 0.1318, 0.1318, 0.1318, 0.1318, 0.…
    ## $ inpower_groups_count       <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, …
    ## $ inpower_groups_pop         <dbl> 0.8562, 0.8562, 0.8562, 0.8562, 0.8562, 0.…
    ## $ regaut_groups_count        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ regaut_group_pop           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ excluded_groups_shift      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ inpower_groups_shift       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ excluded_groups_count_diff <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ excluded_groups_pop_diff   <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.…
    ## $ inpower_groups_count_diff  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ inpower_groups_pop_diff    <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.…

``` r
range(epr$year)
```

    ## [1] 1946 2019
