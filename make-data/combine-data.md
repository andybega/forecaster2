Combine data into states.rds
================

  - [Pieces](#pieces)
      - [Master statelist](#master-statelist)
      - [P\&T coups](#pt-coups)
      - [Make lead DV versions](#make-lead-dv-versions)
  - [Summarize and write output](#summarize-and-write-output)
      - [Variables in data](#variables-in-data)
      - [Missing values by column](#missing-values-by-column)

## Pieces

### Master statelist

``` r
states <- state_panel(1950, 2019, partial = "any")
```

### P\&T coups

``` r
ptcoups <- read_csv("input/ptcoups.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
glimpse(ptcoups)
```

    ## Observations: 11,202
    ## Variables: 20
    ## $ gwcode                      <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    ## $ year                        <dbl> 1950, 1951, 1952, 1953, 1954, 1955, …
    ## $ pt_attempt                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ pt_attempt_num              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ pt_coup_num                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ pt_coup                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ pt_failed_num               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ pt_failed                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ pt_coup_total               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ pt_failed_total             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ pt_attempt_total            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ pt_coup_num5yrs             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ pt_failed_num5yrs           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ pt_attempt_num5yrs          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ pt_coup_num10yrs            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ pt_failed_num10yrs          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ pt_attempt_num10yrs         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ years_since_last_pt_coup    <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1…
    ## $ years_since_last_pt_failed  <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1…
    ## $ years_since_last_pt_attempt <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1…

``` r
plotmiss(ptcoups)
```

![](combine-data_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
states <- left_join(states, ptcoups, by = c("gwcode", "year"))
```

### Make lead DV versions

``` r
dv_vars <- ptcoups %>%
  select(gwcode, year, pt_attempt, pt_coup, pt_failed) 
lead1 <- dv_vars %>%
  mutate(year = year - 1) %>%
  rename(pt_attempt_lead1 = pt_attempt, pt_coup_lead1 = pt_coup, 
         pt_failed_lead1 = pt_failed)
lead2 <- dv_vars %>%
  mutate(year = year - 2) %>%
  rename(pt_attempt_lead2 = pt_attempt, pt_coup_lead2 = pt_coup, 
         pt_failed_lead2 = pt_failed)
dv <- full_join(lead1, lead2)
```

    ## Joining, by = c("gwcode", "year")

Cuba had an attempt in 1952, check data are correct for this:

``` r
cuba <- dv %>%
  filter(gwcode==40 & year < 1953) %>%
  arrange(year) %>%
  select(gwcode, year, pt_coup_lead1, pt_coup_lead2)

cuba
```

    ## # A tibble: 5 x 4
    ##   gwcode  year pt_coup_lead1 pt_coup_lead2
    ##    <dbl> <dbl>         <dbl>         <dbl>
    ## 1     40  1948            NA             0
    ## 2     40  1949             0             0
    ## 3     40  1950             0             1
    ## 4     40  1951             1             0
    ## 5     40  1952             0             0

``` r
stopifnot(cuba$pt_coup_lead1[cuba$year==1951] == 1,
          cuba$pt_coup_lead2[cuba$year==1950] == 1)
```

#### Impute for last 2 years in state existence

Several states drop out during the data period. Set the DV vars to 0 in
those instances.

``` r
last2 <- function(x, yy, ly) {
  ifelse(is.na(x) & yy >= ly - 1, 0L, x)
}

dv_full <- left_join(states, dv) %>%
  select(gwcode, year, contains("lead")) %>%
  group_by(gwcode) %>%
  mutate(last_year = max(year),
         # states with last year in 2019 still exist
         last_year = ifelse(last_year==max(states$year), 9999L, last_year)) %>%
  mutate_at(.vars = vars(contains("lead")), ~last2(., year, last_year)) %>%
  select(-last_year)
```

    ## Joining, by = c("gwcode", "year")

``` r
states <- left_join(states, dv_full)
```

    ## Joining, by = c("gwcode", "year")

## Summarize and write output

``` r
plotmiss(states)
```

![](combine-data_files/figure-gfm/final-missplot-1.png)<!-- -->

Write all incomplete cases to a CSV so changes introduced by something
in one of the input datasets is easier to notice:

``` r
format_years <- function(x) {
  if (length(x) > 1) {
    return(paste(range(x), collapse = " - "))
  }
  as.character(x)
}

incomplete_cases <- states %>%
  gather(var, value, -gwcode, -year) %>%
  mutate(dv_var = str_detect(var, "lead[0-9]{1}")) %>%
  filter( 
    # for non-DV vars, take any missing values
    (!dv_var & is.na(value)) | 
      # for DV vars, last 2 years are missing by design
      (dv_var & is.na(value) & year < max(states$year) - 1)) %>%
  group_by(gwcode, year, var) %>%
  summarize() %>%
  # summarize which vars are missing
  group_by(gwcode, year) %>%
  summarize(missing_values_in = paste0(var, collapse = ", ")) 

# if there are no missing cases, stop; otherwise 
# add in year sequences ID so we can collapse adjacent years with same 
# missing var
if (nrow(incomplete_cases) > 0) {
  incomplete_cases <- incomplete_cases %>%
    group_by(gwcode) %>%
    arrange(year) %>%
    mutate(date = as.Date(paste0(year, "-01-01")),
           yr_id = id_date_sequence(date, "year")) %>%
    group_by(gwcode, yr_id, missing_values_in) %>%
    summarize(years = format_years(year)) %>%
    # clean up
    ungroup() %>%
    select(gwcode, years, missing_values_in) %>%
    arrange(years, gwcode)
}

  
write_csv(incomplete_cases, "output/incomplete-cases.csv")
```

### Variables in data

``` r
var_summary <- states %>%
  pivot_longer(everything(), names_to = "variable") %>%
  group_by(variable) %>%
  summarize(missing = sum(is.na(value)),
            sd = sd(value, na.rm = TRUE),
            integer = all.equal(value, as.integer(value)),
            unique_val_ratio = length(unique(value))/length(value))

write_csv(var_summary, "output/variables.csv")

knitr::kable(var_summary, digits = 2)
```

| variable                        | missing |     sd | integer | unique\_val\_ratio |
| :------------------------------ | ------: | -----: | :------ | -----------------: |
| gwcode                          |       0 | 262.08 | TRUE    |               0.02 |
| pt\_attempt                     |       0 |   0.19 | TRUE    |               0.00 |
| pt\_attempt\_lead1              |     197 |   0.19 | TRUE    |               0.00 |
| pt\_attempt\_lead2              |     394 |   0.19 | TRUE    |               0.00 |
| pt\_attempt\_num                |       0 |   0.23 | TRUE    |               0.00 |
| pt\_attempt\_num10yrs           |       0 |   1.06 | TRUE    |               0.00 |
| pt\_attempt\_num5yrs            |       0 |   0.65 | TRUE    |               0.00 |
| pt\_attempt\_total              |       0 |   1.70 | TRUE    |               0.00 |
| pt\_coup                        |       0 |   0.14 | TRUE    |               0.00 |
| pt\_coup\_lead1                 |     197 |   0.14 | TRUE    |               0.00 |
| pt\_coup\_lead2                 |     394 |   0.14 | TRUE    |               0.00 |
| pt\_coup\_num                   |       0 |   0.15 | TRUE    |               0.00 |
| pt\_coup\_num10yrs              |       0 |   0.60 | TRUE    |               0.00 |
| pt\_coup\_num5yrs               |       0 |   0.39 | TRUE    |               0.00 |
| pt\_coup\_total                 |       0 |   1.70 | TRUE    |               0.00 |
| pt\_failed                      |       0 |   0.14 | TRUE    |               0.00 |
| pt\_failed\_lead1               |     197 |   0.14 | TRUE    |               0.00 |
| pt\_failed\_lead2               |     394 |   0.14 | TRUE    |               0.00 |
| pt\_failed\_num                 |       0 |   0.16 | TRUE    |               0.00 |
| pt\_failed\_num10yrs            |       0 |   0.66 | TRUE    |               0.00 |
| pt\_failed\_num5yrs             |       0 |   0.43 | TRUE    |               0.00 |
| pt\_failed\_total               |       0 |   1.87 | TRUE    |               0.00 |
| year                            |       0 |  19.09 | TRUE    |               0.01 |
| years\_since\_last\_pt\_attempt |       0 |  18.07 | TRUE    |               0.01 |
| years\_since\_last\_pt\_coup    |       0 |  18.32 | TRUE    |               0.01 |
| years\_since\_last\_pt\_failed  |       0 |  18.14 | TRUE    |               0.01 |

### Missing values by column

``` r
sapply(states, function(x) sum(is.na(x))) %>%
  as.list() %>%
  tibble::enframe(name = "Variable", value = "Missing") %>%
  unnest(Missing) %>%
  filter(Missing > 0) %>%
  knitr::kable()
```

| Variable           | Missing |
| :----------------- | ------: |
| pt\_attempt\_lead1 |     197 |
| pt\_coup\_lead1    |     197 |
| pt\_failed\_lead1  |     197 |
| pt\_attempt\_lead2 |     394 |
| pt\_coup\_lead2    |     394 |
| pt\_failed\_lead2  |     394 |

``` r
write_rds(states, "output/states.rds")
```
