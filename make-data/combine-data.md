Combine data into states.rds
================

  - [Pieces](#pieces)
      - [Master statelist](#master-statelist)
      - [P\&T coups](#pt-coups)
      - [Make lead DV versions](#make-lead-dv-versions)
  - [G\&W state age](#gw-state-age)
  - [EPR](#epr)
  - [REIGN data](#reign-data)
  - [V-Dem](#v-dem)
  - [Infant mortality](#infant-mortality)
  - [Summarize and write output](#summarize-and-write-output)
      - [Variables in data](#variables-in-data)
      - [Missing values by column](#missing-values-by-column)
      - [Track overall cases and missing
        cases](#track-overall-cases-and-missing-cases)
      - [Track forecast sets](#track-forecast-sets)
  - [Save](#save)

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

    ## Rows: 11,202
    ## Columns: 20
    ## $ gwcode                      <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
    ## $ year                        <dbl> 1950, 1951, 1952, 1953, 1954, 1955, 1956,…
    ## $ pt_attempt                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ pt_attempt_num              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ pt_coup_num                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ pt_coup                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ pt_failed_num               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ pt_failed                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ pt_coup_total               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ pt_failed_total             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ pt_attempt_total            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ pt_coup_num5yrs             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ pt_failed_num5yrs           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ pt_attempt_num5yrs          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ pt_coup_num10yrs            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ pt_failed_num10yrs          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ pt_attempt_num10yrs         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ years_since_last_pt_coup    <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13…
    ## $ years_since_last_pt_failed  <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13…
    ## $ years_since_last_pt_attempt <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13…

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

## G\&W state age

Years since independence

``` r
age <- read_csv("input/gwstate-age.csv") %>%
  mutate(ln_state_age = log(state_age)) %>%
  select(-state_age)
```

    ## Parsed with column specification:
    ## cols(
    ##   gwcode = col_double(),
    ##   year = col_double(),
    ##   state_age = col_double()
    ## )

``` r
states <- left_join(states, age, by = c("gwcode", "year"))
```

## EPR

Ethnic Power Relations data on ethnic groups in countries.

``` r
epr <- read_csv("input/epr.csv") %>%
  setNames(c("gwcode", "year", paste0("epr_", names(.)[3:ncol(.)])))
```

    ## Parsed with column specification:
    ## cols(
    ##   gwcode = col_double(),
    ##   year = col_double(),
    ##   groups = col_double(),
    ##   elf = col_double(),
    ##   excluded_groups_count = col_double(),
    ##   excluded_groups_pop = col_double(),
    ##   inpower_groups_count = col_double(),
    ##   inpower_groups_pop = col_double(),
    ##   regaut_groups_count = col_double(),
    ##   regaut_group_pop = col_double(),
    ##   excluded_groups_shift = col_double(),
    ##   inpower_groups_shift = col_double(),
    ##   excluded_groups_count_diff = col_double(),
    ##   excluded_groups_pop_diff = col_double(),
    ##   inpower_groups_count_diff = col_double(),
    ##   inpower_groups_pop_diff = col_double()
    ## )

``` r
glimpse(epr)
```

    ## Rows: 10,488
    ## Columns: 16
    ## $ gwcode                         <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
    ## $ year                           <dbl> 1946, 1947, 1948, 1949, 1950, 1951, 19…
    ## $ epr_groups                     <dbl> 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,…
    ## $ epr_elf                        <dbl> 0.5098565, 0.5098565, 0.5098565, 0.509…
    ## $ epr_excluded_groups_count      <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
    ## $ epr_excluded_groups_pop        <dbl> 0.1318, 0.1318, 0.1318, 0.1318, 0.1318…
    ## $ epr_inpower_groups_count       <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,…
    ## $ epr_inpower_groups_pop         <dbl> 0.8562, 0.8562, 0.8562, 0.8562, 0.8562…
    ## $ epr_regaut_groups_count        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ epr_regaut_group_pop           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ epr_excluded_groups_shift      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ epr_inpower_groups_shift       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ epr_excluded_groups_count_diff <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ epr_excluded_groups_pop_diff   <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000…
    ## $ epr_inpower_groups_count_diff  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ epr_inpower_groups_pop_diff    <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000…

``` r
states <- left_join(states, epr, by = c("gwcode", "year"))
```

## REIGN data

``` r
reign <- read_csv("input/reign-cy.csv") %>%
  setNames(., c("gwcode", "year", paste0("reign_", names(.)[3:ncol(.)])))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   government = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
glimpse(reign)
```

    ## Rows: 11,031
    ## Columns: 30
    ## $ gwcode                <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
    ## $ year                  <dbl> 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957,…
    ## $ reign_elected         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1,…
    ## $ reign_age             <dbl> 66, 67, 68, 63, 64, 65, 66, 67, 68, 69, 70, 44,…
    ## $ reign_male            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ reign_militarycareer  <dbl> 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,…
    ## $ reign_tenure_months   <dbl> 69, 81, 93, 12, 24, 36, 48, 60, 72, 84, 96, 12,…
    ## $ reign_government      <chr> "Presidential Democracy", "Presidential Democra…
    ## $ reign_anticipation    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ reign_ref_ant         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ reign_leg_ant         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ reign_exec_ant        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ reign_irreg_lead_ant  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ reign_election_now    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ reign_election_recent <dbl> 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0,…
    ## $ reign_leg_recent      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ reign_exec_recent     <dbl> 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0,…
    ## $ reign_lead_recent     <dbl> 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0,…
    ## $ reign_ref_recent      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ reign_direct_recent   <dbl> 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0,…
    ## $ reign_indirect_recent <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ reign_victory_recent  <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0,…
    ## $ reign_defeat_recent   <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,…
    ## $ reign_change_recent   <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,…
    ## $ reign_nochange_recent <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0,…
    ## $ reign_delayed         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ reign_lastelection    <dbl> 3.2188759, 3.6109178, 0.6931472, 2.6390574, 3.2…
    ## $ reign_loss            <dbl> 5.3798971, 5.4337220, 0.6931472, 2.6390574, 3.2…
    ## $ reign_irregular       <dbl> 7.571474, 7.577634, 7.583756, 7.589842, 7.59589…
    ## $ reign_prev_conflict   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…

``` r
# Simplify the government coding
table(reign$reign_government) %>% sort() %>% rev()
```

    ## 
    ##        Parliamentary Democracy         Presidential Democracy 
    ##                           3371                           1806 
    ##                 Dominant Party          Personal Dictatorship 
    ##                           1655                           1458 
    ##                       Monarchy                 Party-Personal 
    ##                            977                            437 
    ##                       Military              Military-Personal 
    ##                            308                            284 
    ## Party-Personal-Military Hybrid               Foreign/Occupied 
    ##                            191                            147 
    ##                 Party-Military         Provisional - Civilian 
    ##                            144                             74 
    ##                      Oligarchy                     Warlordism 
    ##                             67                             45 
    ##              Indirect Military         Provisional - Military 
    ##                             41                             26

``` r
check <- left_join(states, reign)
```

    ## Joining, by = c("gwcode", "year")

``` r
check %>% 
  mutate(pt_attempt_lead1 = as.integer(as.character(pt_attempt_lead1)),
         pt_coup_lead1 = as.integer(as.character(pt_coup_lead1))) %>%
  group_by(reign_government) %>% 
  summarize(n = n(), 
            attempts = sum(pt_attempt_lead1, na.rm = TRUE), 
            attempt_rate = mean(pt_attempt_lead1, na.rm = TRUE), 
            coup = sum(pt_coup_lead1, na.rm = TRUE), 
            coup_rate = mean(pt_coup_lead1, na.rm = TRUE)) %>% 
  arrange(desc(attempt_rate))
```

    ## # A tibble: 17 x 6
    ##    reign_government                   n attempts attempt_rate  coup coup_rate
    ##    <chr>                          <int>    <int>        <dbl> <int>     <dbl>
    ##  1 Indirect Military                 41       10      0.244       5   0.122  
    ##  2 Provisional - Military            26        6      0.231       4   0.154  
    ##  3 Provisional - Civilian            74       13      0.186       7   0.1    
    ##  4 Military                         308       42      0.137      32   0.104  
    ##  5 Military-Personal                284       34      0.121      13   0.0461 
    ##  6 Warlordism                        45        5      0.116       1   0.0233 
    ##  7 Party-Military                   144       12      0.0845      6   0.0423 
    ##  8 Personal Dictatorship           1458      100      0.0694     46   0.0319 
    ##  9 Party-Personal-Military Hybrid   191        8      0.0421      6   0.0316 
    ## 10 Presidential Democracy          1806       56      0.0319     28   0.0160 
    ## 11 Oligarchy                         67        2      0.0299      2   0.0299 
    ## 12 Party-Personal                   437       12      0.0279      6   0.0140 
    ## 13 Foreign/Occupied                 147        4      0.0276      4   0.0276 
    ## 14 Monarchy                         977       23      0.0239     12   0.0124 
    ## 15 Dominant Party                  1655       37      0.0225     21   0.0128 
    ## 16 Parliamentary Democracy         3371       40      0.0121     24   0.00728
    ## 17 <NA>                             171        1      0.00595     1   0.00595

``` r
reign <- reign %>%
  mutate(
    reign_gov_pres = as.integer(reign_government=="Presidential Democracy"),
    reign_gov_parl = as.integer(reign_government=="Parliamentary Democracy"),
    reign_gov_personal = as.integer(reign_government %in% c("Personal Dictatorship", "Monarchy")),
    reign_gov_party = as.integer(reign_government %in% c("Dominant Party", "Party-Personal", "Party-Personal-Military Hybrid", "Party-Military")),
    reign_gov_provisional = as.integer(str_detect(reign_government, "Provisional")),
    reign_gov_military = as.integer(reign_government %in% c("Indirect Military", "Military", "Warlordism", "Military-Personal"))
  ) %>%
  select(-reign_government)

states <- left_join(states, reign)
```

    ## Joining, by = c("gwcode", "year")

## V-Dem

``` r
vdem <- read_csv("input/v-dem.csv",
                 col_types = cols(
                   .default = col_double(),
                   country_name = col_character(),
                   year = col_integer()
                 )) %>%
  select(-country_name, -spell_id) %>%
  filter(!is.na(gwcode)) %>%
  setNames(c("gwcode", "year", paste0("vdem_", names(.)[3:ncol(.)])))

states <- left_join(states, vdem)
```

    ## Joining, by = c("gwcode", "year")

## Infant mortality

``` r
wdi <- read_csv("input/wdi-infmort.csv",
                 col_types = cols(
                   .default = col_double(),
                   year = col_integer(),
                   infmort_imputed = col_logical()
                 )) %>%
  mutate(infmort_imputed = as.integer(infmort_imputed)) %>%
  setNames(c("gwcode", "year", paste0("wdi_", names(.)[3:ncol(.)])))

states <- left_join(states, wdi)
```

    ## Joining, by = c("gwcode", "year")

## Summarize and write output

``` r
plotmiss(states)
```

![](combine-data_files/figure-gfm/final-missplot-1.png)<!-- -->

WDI does not start until 1960, so it is missing for the 50s. This clogs
up the missing cases logs I’m creating below and makes it harder to ID
actual cases I could try to fix. Take out the 50s data before going over
the data summaries.

``` r
states <- states[states$year >= 1960, ]
```

Write all incomplete cases to a CSV so changes introduced by something
in one of the input datasets is easier to notice:

``` r
format_years <- function(x) {
  if (length(x) > 1) {
    return(paste(range(x), collapse = " - "))
  }
  as.character(x)
}

incomplete_cases_by_var <- states %>%
  gather(var, value, -gwcode, -year) %>%
  mutate(dv_var = str_detect(var, "lead[0-9]{1}")) %>%
  filter( 
    # for non-DV vars, take any missing values
    (!dv_var & is.na(value)) | 
      # for DV vars, last 2 years are missing by design
      (dv_var & is.na(value) & year < max(states$year) - 1)) %>%
  group_by(gwcode, year, var) %>%
  summarize() 

# the by_var version above is big and inefficient; change it so that we 
# collapse over year ranges, and so that instead of tracking each specific 
# variable, we only identify the data source responsible
incomplete_cases <- incomplete_cases_by_var %>%
  # collapse vars to data source, using the prefix
  mutate(var = str_replace(var, "^([a-z]+)\\_[a-zA-Z0-9\\_]+$", "\\1")) %>%
  group_by(gwcode, year, var) %>%
  summarize() %>%
  # Before we can collapse over year ranges, we need to get the dataset 
  # so that there are no repeated years for a country, otherwise we can't 
  # ID correct [country, missing sources] sets. 
  # Thus, collapse the missing data source info
  group_by(gwcode, year) %>%
  summarize(data_sources = paste0(sort(unique(var)), collapse = "; ")) %>%
  # Collapse year ranges; first we need to id successive year spells so that
  # we don't collapse over gaps, e.g. when a country lost independence during 
  # WW2
  group_by(gwcode) %>%
  arrange(year) %>%
  mutate(spell_id = id_date_sequence(year)) %>%
  # Now collapse over years
  group_by(gwcode, spell_id, data_sources) %>%
  summarize(years = format_years(year),
            N = n()) %>%
  # re-order so it is easier to read
  arrange(gwcode, spell_id) %>%
  select(gwcode, spell_id, years, N, data_sources)
  
write_csv(incomplete_cases, "output/incomplete-cases.csv")
```

### Variables in data

``` r
var_summary <- states %>%
  pivot_longer(everything(), names_to = "variable") %>%
  group_by(variable) %>%
  summarize(missing = sum(is.na(value)),
            sd = sd(value, na.rm = TRUE),
            integer = isTRUE(all.equal(value, as.integer(value))),
            unique_val_ratio = length(unique(value))/length(value))

write_csv(var_summary, "output/variables.csv")

knitr::kable(var_summary, digits = 2)
```

| variable                           | missing |     sd | integer | unique\_val\_ratio |
| :--------------------------------- | ------: | -----: | :------ | -----------------: |
| epr\_elf                           |     978 |   0.30 | FALSE   |               0.03 |
| epr\_excluded\_groups\_count       |     978 |   4.95 | TRUE    |               0.00 |
| epr\_excluded\_groups\_count\_diff |     978 |   0.42 | TRUE    |               0.00 |
| epr\_excluded\_groups\_pop         |     978 |   0.22 | FALSE   |               0.03 |
| epr\_excluded\_groups\_pop\_diff   |     978 |   0.05 | FALSE   |               0.02 |
| epr\_excluded\_groups\_shift       |     978 |   0.17 | TRUE    |               0.00 |
| epr\_groups                        |     978 |   5.63 | TRUE    |               0.00 |
| epr\_inpower\_groups\_count        |     978 |   2.17 | TRUE    |               0.00 |
| epr\_inpower\_groups\_count\_diff  |     978 |   0.39 | TRUE    |               0.00 |
| epr\_inpower\_groups\_pop          |     978 |   0.26 | FALSE   |               0.03 |
| epr\_inpower\_groups\_pop\_diff    |     978 |   0.05 | FALSE   |               0.02 |
| epr\_inpower\_groups\_shift        |     978 |   0.17 | TRUE    |               0.00 |
| epr\_regaut\_group\_pop            |     978 |   0.00 | TRUE    |               0.00 |
| epr\_regaut\_groups\_count         |     978 |   0.00 | TRUE    |               0.00 |
| gwcode                             |       0 | 261.20 | TRUE    |               0.02 |
| ln\_state\_age                     |       0 |   1.14 | FALSE   |               0.02 |
| pt\_attempt                        |       0 |   0.18 | TRUE    |               0.00 |
| pt\_attempt\_lead1                 |     197 |   0.18 | TRUE    |               0.00 |
| pt\_attempt\_lead2                 |     394 |   0.18 | TRUE    |               0.00 |
| pt\_attempt\_num                   |       0 |   0.23 | TRUE    |               0.00 |
| pt\_attempt\_num10yrs              |       0 |   1.07 | TRUE    |               0.00 |
| pt\_attempt\_num5yrs               |       0 |   0.65 | TRUE    |               0.00 |
| pt\_attempt\_total                 |       0 |   1.75 | TRUE    |               0.00 |
| pt\_coup                           |       0 |   0.14 | TRUE    |               0.00 |
| pt\_coup\_lead1                    |     197 |   0.14 | TRUE    |               0.00 |
| pt\_coup\_lead2                    |     394 |   0.13 | TRUE    |               0.00 |
| pt\_coup\_num                      |       0 |   0.15 | TRUE    |               0.00 |
| pt\_coup\_num10yrs                 |       0 |   0.61 | TRUE    |               0.00 |
| pt\_coup\_num5yrs                  |       0 |   0.38 | TRUE    |               0.00 |
| pt\_coup\_total                    |       0 |   1.75 | TRUE    |               0.00 |
| pt\_failed                         |       0 |   0.13 | TRUE    |               0.00 |
| pt\_failed\_lead1                  |     197 |   0.13 | TRUE    |               0.00 |
| pt\_failed\_lead2                  |     394 |   0.13 | TRUE    |               0.00 |
| pt\_failed\_num                    |       0 |   0.16 | TRUE    |               0.00 |
| pt\_failed\_num10yrs               |       0 |   0.67 | TRUE    |               0.00 |
| pt\_failed\_num5yrs                |       0 |   0.43 | TRUE    |               0.00 |
| pt\_failed\_total                  |       0 |   1.93 | TRUE    |               0.00 |
| reign\_age                         |     138 |  11.14 | TRUE    |               0.01 |
| reign\_anticipation                |     138 |   0.31 | TRUE    |               0.00 |
| reign\_change\_recent              |     138 |   0.20 | TRUE    |               0.00 |
| reign\_defeat\_recent              |     138 |   0.19 | TRUE    |               0.00 |
| reign\_delayed                     |     138 |   0.09 | TRUE    |               0.00 |
| reign\_direct\_recent              |     138 |   0.30 | TRUE    |               0.00 |
| reign\_elected                     |     138 |   0.47 | TRUE    |               0.00 |
| reign\_election\_now               |     138 |   0.15 | TRUE    |               0.00 |
| reign\_election\_recent            |     138 |   0.31 | TRUE    |               0.00 |
| reign\_exec\_ant                   |     138 |   0.23 | TRUE    |               0.00 |
| reign\_exec\_recent                |     138 |   0.21 | TRUE    |               0.00 |
| reign\_gov\_military               |     138 |   0.24 | TRUE    |               0.00 |
| reign\_gov\_parl                   |     138 |   0.46 | TRUE    |               0.00 |
| reign\_gov\_party                  |     138 |   0.41 | TRUE    |               0.00 |
| reign\_gov\_personal               |     138 |   0.41 | TRUE    |               0.00 |
| reign\_gov\_pres                   |     138 |   0.37 | TRUE    |               0.00 |
| reign\_gov\_provisional            |     138 |   0.10 | TRUE    |               0.00 |
| reign\_indirect\_recent            |     138 |   0.09 | TRUE    |               0.00 |
| reign\_irreg\_lead\_ant            |     138 |   0.13 | TRUE    |               0.00 |
| reign\_irregular                   |     138 |   1.46 | FALSE   |               0.12 |
| reign\_lastelection                |     138 |   1.33 | FALSE   |               0.07 |
| reign\_lead\_recent                |     138 |   0.29 | TRUE    |               0.00 |
| reign\_leg\_ant                    |     138 |   0.20 | TRUE    |               0.00 |
| reign\_leg\_recent                 |     138 |   0.21 | TRUE    |               0.00 |
| reign\_loss                        |     138 |   1.39 | FALSE   |               0.09 |
| reign\_male                        |     138 |   0.17 | TRUE    |               0.00 |
| reign\_militarycareer              |     138 |   0.40 | TRUE    |               0.00 |
| reign\_nochange\_recent            |     138 |   0.24 | TRUE    |               0.00 |
| reign\_prev\_conflict              |     138 |   0.60 | TRUE    |               0.00 |
| reign\_ref\_ant                    |     138 |   0.10 | TRUE    |               0.00 |
| reign\_ref\_recent                 |     138 |   0.11 | TRUE    |               0.00 |
| reign\_tenure\_months              |     138 |  98.30 | TRUE    |               0.05 |
| reign\_victory\_recent             |     138 |   0.25 | TRUE    |               0.00 |
| vdem\_v2x\_accountability          |     969 |   1.01 | FALSE   |               0.31 |
| vdem\_v2x\_api                     |     988 |   0.28 | FALSE   |               0.09 |
| vdem\_v2x\_civlib                  |     969 |   0.29 | FALSE   |               0.09 |
| vdem\_v2x\_clphy                   |     969 |   0.31 | FALSE   |               0.09 |
| vdem\_v2x\_clpol                   |     969 |   0.33 | FALSE   |               0.09 |
| vdem\_v2x\_clpriv                  |     969 |   0.29 | FALSE   |               0.09 |
| vdem\_v2x\_corr                    |    1003 |   0.29 | FALSE   |               0.09 |
| vdem\_v2x\_cspart                  |     969 |   0.29 | FALSE   |               0.09 |
| vdem\_v2x\_delibdem                |     988 |   0.27 | FALSE   |               0.09 |
| vdem\_v2x\_diagacc                 |     969 |   1.04 | FALSE   |               0.32 |
| vdem\_v2x\_divparctrl              |    1007 |   0.97 | FALSE   |               0.16 |
| vdem\_v2x\_EDcomp\_thick           |     988 |   0.29 | FALSE   |               0.09 |
| vdem\_v2x\_egal                    |     969 |   0.23 | FALSE   |               0.09 |
| vdem\_v2x\_egaldem                 |     988 |   0.25 | FALSE   |               0.08 |
| vdem\_v2x\_elecoff                 |     969 |   0.41 | FALSE   |               0.01 |
| vdem\_v2x\_elecreg                 |     969 |   0.35 | TRUE    |               0.00 |
| vdem\_v2x\_ex\_confidence          |     969 |   0.38 | FALSE   |               0.00 |
| vdem\_v2x\_ex\_direlect            |     969 |   0.47 | FALSE   |               0.00 |
| vdem\_v2x\_ex\_hereditary          |     969 |   0.16 | FALSE   |               0.01 |
| vdem\_v2x\_ex\_military            |     969 |   0.24 | FALSE   |               0.01 |
| vdem\_v2x\_ex\_party               |     969 |   0.21 | FALSE   |               0.01 |
| vdem\_v2x\_execorr                 |     969 |   0.30 | FALSE   |               0.08 |
| vdem\_v2x\_feduni                  |     970 |   0.36 | FALSE   |               0.06 |
| vdem\_v2x\_frassoc\_thick          |     969 |   0.34 | FALSE   |               0.09 |
| vdem\_v2x\_freexp                  |     969 |   0.32 | FALSE   |               0.09 |
| vdem\_v2x\_freexp\_altinf          |     969 |   0.33 | FALSE   |               0.09 |
| vdem\_v2x\_gencl                   |     969 |   0.27 | FALSE   |               0.09 |
| vdem\_v2x\_gencs                   |     969 |   0.25 | FALSE   |               0.09 |
| vdem\_v2x\_gender                  |    1125 |   0.23 | FALSE   |               0.08 |
| vdem\_v2x\_genpp                   |    1125 |   0.27 | FALSE   |               0.08 |
| vdem\_v2x\_horacc                  |     969 |   1.04 | FALSE   |               0.33 |
| vdem\_v2x\_hosabort                |     969 |   0.06 | TRUE    |               0.00 |
| vdem\_v2x\_hosinter                |     969 |   0.10 | TRUE    |               0.00 |
| vdem\_v2x\_jucon                   |    1000 |   0.31 | FALSE   |               0.09 |
| vdem\_v2x\_legabort                |     969 |   0.05 | TRUE    |               0.00 |
| vdem\_v2x\_libdem                  |    1021 |   0.28 | FALSE   |               0.09 |
| vdem\_v2x\_liberal                 |    1002 |   0.29 | FALSE   |               0.09 |
| vdem\_v2x\_mpi                     |     988 |   0.31 | FALSE   |               0.08 |
| vdem\_v2x\_neopat                  |     970 |   0.31 | FALSE   |               0.09 |
| vdem\_v2x\_partip                  |     969 |   0.21 | FALSE   |               0.08 |
| vdem\_v2x\_partipdem               |     988 |   0.21 | FALSE   |               0.07 |
| vdem\_v2x\_polyarchy               |     988 |   0.29 | FALSE   |               0.09 |
| vdem\_v2x\_pubcorr                 |     969 |   0.30 | FALSE   |               0.08 |
| vdem\_v2x\_rule                    |     969 |   0.31 | FALSE   |               0.09 |
| vdem\_v2x\_suffr                   |     969 |   0.19 | FALSE   |               0.00 |
| vdem\_v2x\_veracc                  |     969 |   0.87 | FALSE   |               0.26 |
| wdi\_infmort                       |    1094 |  50.02 | FALSE   |               0.21 |
| wdi\_infmort\_imputed              |    1094 |   0.22 | TRUE    |               0.00 |
| wdi\_infmort\_yearadj              |    1094 |   1.00 | FALSE   |               0.81 |
| year                               |       0 |  16.84 | TRUE    |               0.01 |
| years\_since\_last\_pt\_attempt    |       0 |  18.10 | TRUE    |               0.01 |
| years\_since\_last\_pt\_coup       |       0 |  18.17 | TRUE    |               0.01 |
| years\_since\_last\_pt\_failed     |       0 |  18.00 | TRUE    |               0.01 |

### Missing values by column

``` r
sapply(states, function(x) sum(is.na(x))) %>%
  as.list() %>%
  tibble::enframe(name = "Variable", value = "Missing") %>%
  unnest(Missing) %>%
  filter(Missing > 0) %>%
  knitr::kable()
```

| Variable                           | Missing |
| :--------------------------------- | ------: |
| pt\_attempt\_lead1                 |     197 |
| pt\_coup\_lead1                    |     197 |
| pt\_failed\_lead1                  |     197 |
| pt\_attempt\_lead2                 |     394 |
| pt\_coup\_lead2                    |     394 |
| pt\_failed\_lead2                  |     394 |
| epr\_groups                        |     978 |
| epr\_elf                           |     978 |
| epr\_excluded\_groups\_count       |     978 |
| epr\_excluded\_groups\_pop         |     978 |
| epr\_inpower\_groups\_count        |     978 |
| epr\_inpower\_groups\_pop          |     978 |
| epr\_regaut\_groups\_count         |     978 |
| epr\_regaut\_group\_pop            |     978 |
| epr\_excluded\_groups\_shift       |     978 |
| epr\_inpower\_groups\_shift        |     978 |
| epr\_excluded\_groups\_count\_diff |     978 |
| epr\_excluded\_groups\_pop\_diff   |     978 |
| epr\_inpower\_groups\_count\_diff  |     978 |
| epr\_inpower\_groups\_pop\_diff    |     978 |
| reign\_elected                     |     138 |
| reign\_age                         |     138 |
| reign\_male                        |     138 |
| reign\_militarycareer              |     138 |
| reign\_tenure\_months              |     138 |
| reign\_anticipation                |     138 |
| reign\_ref\_ant                    |     138 |
| reign\_leg\_ant                    |     138 |
| reign\_exec\_ant                   |     138 |
| reign\_irreg\_lead\_ant            |     138 |
| reign\_election\_now               |     138 |
| reign\_election\_recent            |     138 |
| reign\_leg\_recent                 |     138 |
| reign\_exec\_recent                |     138 |
| reign\_lead\_recent                |     138 |
| reign\_ref\_recent                 |     138 |
| reign\_direct\_recent              |     138 |
| reign\_indirect\_recent            |     138 |
| reign\_victory\_recent             |     138 |
| reign\_defeat\_recent              |     138 |
| reign\_change\_recent              |     138 |
| reign\_nochange\_recent            |     138 |
| reign\_delayed                     |     138 |
| reign\_lastelection                |     138 |
| reign\_loss                        |     138 |
| reign\_irregular                   |     138 |
| reign\_prev\_conflict              |     138 |
| reign\_gov\_pres                   |     138 |
| reign\_gov\_parl                   |     138 |
| reign\_gov\_personal               |     138 |
| reign\_gov\_party                  |     138 |
| reign\_gov\_provisional            |     138 |
| reign\_gov\_military               |     138 |
| vdem\_v2x\_polyarchy               |     988 |
| vdem\_v2x\_libdem                  |    1021 |
| vdem\_v2x\_partipdem               |     988 |
| vdem\_v2x\_delibdem                |     988 |
| vdem\_v2x\_egaldem                 |     988 |
| vdem\_v2x\_api                     |     988 |
| vdem\_v2x\_mpi                     |     988 |
| vdem\_v2x\_freexp\_altinf          |     969 |
| vdem\_v2x\_frassoc\_thick          |     969 |
| vdem\_v2x\_suffr                   |     969 |
| vdem\_v2x\_elecoff                 |     969 |
| vdem\_v2x\_liberal                 |    1002 |
| vdem\_v2x\_jucon                   |    1000 |
| vdem\_v2x\_partip                  |     969 |
| vdem\_v2x\_cspart                  |     969 |
| vdem\_v2x\_egal                    |     969 |
| vdem\_v2x\_accountability          |     969 |
| vdem\_v2x\_veracc                  |     969 |
| vdem\_v2x\_diagacc                 |     969 |
| vdem\_v2x\_horacc                  |     969 |
| vdem\_v2x\_ex\_confidence          |     969 |
| vdem\_v2x\_ex\_direlect            |     969 |
| vdem\_v2x\_ex\_hereditary          |     969 |
| vdem\_v2x\_ex\_military            |     969 |
| vdem\_v2x\_ex\_party               |     969 |
| vdem\_v2x\_neopat                  |     970 |
| vdem\_v2x\_civlib                  |     969 |
| vdem\_v2x\_clphy                   |     969 |
| vdem\_v2x\_clpol                   |     969 |
| vdem\_v2x\_clpriv                  |     969 |
| vdem\_v2x\_corr                    |    1003 |
| vdem\_v2x\_execorr                 |     969 |
| vdem\_v2x\_pubcorr                 |     969 |
| vdem\_v2x\_gender                  |    1125 |
| vdem\_v2x\_gencl                   |     969 |
| vdem\_v2x\_gencs                   |     969 |
| vdem\_v2x\_genpp                   |    1125 |
| vdem\_v2x\_rule                    |     969 |
| vdem\_v2x\_elecreg                 |     969 |
| vdem\_v2x\_EDcomp\_thick           |     988 |
| vdem\_v2x\_freexp                  |     969 |
| vdem\_v2x\_hosabort                |     969 |
| vdem\_v2x\_hosinter                |     969 |
| vdem\_v2x\_legabort                |     969 |
| vdem\_v2x\_divparctrl              |    1007 |
| vdem\_v2x\_feduni                  |     970 |
| wdi\_infmort                       |    1094 |
| wdi\_infmort\_yearadj              |    1094 |
| wdi\_infmort\_imputed              |    1094 |

### Track overall cases and missing cases

``` r
# The DV values for the last year or two will be missing, but those are not
# cases we want to drop as they still serve as prediction targets. So run 
# complete.cases on the data without DVs so we can get an index for rows to 
# drop
no_dv <- states %>%
  select(-ends_with(c("lead1", "lead2")))
drop_idx <- !complete.cases(no_dv)

# Write out high-level summary stats
tbl <- list(
  N_before_drop = nrow(states),
  N_after_drop  = sum(drop_idx==FALSE),
  Years = format_years(states[!drop_idx, ][["year"]]),
  Features = as.integer(ncol(no_dv) - 2),
  Positive_attempt_lead1 = as.integer(
    sum(states[!drop_idx, ][["pt_attempt_lead1"]], na.rm = TRUE)
  ),
  Positive_coup_lead1 = as.integer(
    sum(states[!drop_idx, ][["pt_coup_lead1"]], na.rm = TRUE)
  ),
  Positive_failed_lead1 = as.integer(
    sum(states[!drop_idx, ][["pt_failed_lead1"]], na.rm = TRUE)
  ),
  N_in_forecast_sets = nrow(states[!drop_idx & states$year %in% 2010:2019, ])
)

tbl %>% 
  enframe("Measure", "Value") %>% 
  mutate(Value = as.character(Value)) %>% 
  knitr::kable()
```

| Measure                  | Value       |
| :----------------------- | :---------- |
| N\_before\_drop          | 10297       |
| N\_after\_drop           | 8754        |
| Years                    | 1960 - 2019 |
| Features                 | 115         |
| Positive\_attempt\_lead1 | 323         |
| Positive\_coup\_lead1    | 169         |
| Positive\_failed\_lead1  | 171         |
| N\_in\_forecast\_sets    | 1686        |

``` r
tbl %>%
  yaml::as.yaml() %>%
  writeLines("output/data-summary.yml")
```

### Track forecast sets

``` r
years         <- 2010:2019
forecast_data <- states[!drop_idx & states$year %in% years, c("gwcode", "year")]

gw_full <- state_panel(min(years), max(years), partial = "any")

# Which countries are covered in each forecast set
covered <- forecast_data %>%
  # ID consecutive year sequences
  group_by(gwcode) %>%
  arrange(gwcode, year) %>%
  mutate(spell = id_date_sequence(year)) %>%
  # collapse over consecutive years
  group_by(gwcode, spell) %>%
  summarize(years = format_years(year)) %>%
  # add country names
  mutate(country = country_names(gwcode, shorten = TRUE)) %>%
  select(gwcode, country, years)
covered %>% 
  knitr::kable(caption = "Countries covered by the test and live forecasts")
```

| gwcode | country                  | years       |
| -----: | :----------------------- | :---------- |
|      2 | United States of America | 2010 - 2019 |
|     20 | Canada                   | 2010 - 2019 |
|     40 | Cuba                     | 2010 - 2019 |
|     41 | Haiti                    | 2010 - 2019 |
|     42 | Dominican Republic       | 2010 - 2019 |
|     51 | Jamaica                  | 2010 - 2019 |
|     52 | Trinidad and Tobago      | 2010 - 2019 |
|     53 | Barbados                 | 2010 - 2019 |
|     70 | Mexico                   | 2010 - 2019 |
|     90 | Guatemala                | 2010 - 2019 |
|     91 | Honduras                 | 2010 - 2019 |
|     92 | El Salvador              | 2010 - 2019 |
|     93 | Nicaragua                | 2010 - 2019 |
|     94 | Costa Rica               | 2010 - 2019 |
|     95 | Panama                   | 2010 - 2019 |
|    100 | Colombia                 | 2010 - 2019 |
|    101 | Venezuela                | 2010 - 2019 |
|    110 | Guyana                   | 2010 - 2019 |
|    115 | Surinam                  | 2010 - 2019 |
|    130 | Ecuador                  | 2010 - 2019 |
|    135 | Peru                     | 2010 - 2019 |
|    140 | Brazil                   | 2010 - 2019 |
|    145 | Bolivia                  | 2010 - 2019 |
|    150 | Paraguay                 | 2010 - 2019 |
|    155 | Chile                    | 2010 - 2019 |
|    160 | Argentina                | 2010 - 2019 |
|    165 | Uruguay                  | 2010 - 2019 |
|    200 | United Kingdom           | 2010 - 2019 |
|    205 | Ireland                  | 2010 - 2019 |
|    210 | Netherlands              | 2010 - 2019 |
|    211 | Belgium                  | 2010 - 2019 |
|    212 | Luxembourg               | 2010 - 2019 |
|    220 | France                   | 2010 - 2019 |
|    225 | Switzerland              | 2010 - 2019 |
|    230 | Spain                    | 2010 - 2019 |
|    235 | Portugal                 | 2010 - 2019 |
|    260 | German Federal Republic  | 2010 - 2019 |
|    290 | Poland                   | 2010 - 2019 |
|    305 | Austria                  | 2010 - 2019 |
|    310 | Hungary                  | 2010 - 2019 |
|    316 | Czech Republic           | 2010 - 2019 |
|    317 | Slovakia                 | 2010 - 2019 |
|    325 | Italy/Sardinia           | 2010 - 2019 |
|    338 | Malta                    | 2010 - 2019 |
|    339 | Albania                  | 2010 - 2019 |
|    340 | Serbia                   | 2010 - 2019 |
|    341 | Montenegro               | 2010 - 2019 |
|    343 | North Macedonia          | 2010 - 2019 |
|    344 | Croatia                  | 2010 - 2019 |
|    346 | Bosnia-Herzegovina       | 2010 - 2019 |
|    349 | Slovenia                 | 2010 - 2019 |
|    350 | Greece                   | 2010 - 2019 |
|    352 | Cyprus                   | 2010 - 2019 |
|    355 | Bulgaria                 | 2010 - 2019 |
|    359 | Moldova                  | 2010 - 2019 |
|    360 | Rumania                  | 2010 - 2019 |
|    365 | Russia (Soviet Union)    | 2010 - 2019 |
|    366 | Estonia                  | 2010 - 2019 |
|    367 | Latvia                   | 2010 - 2019 |
|    368 | Lithuania                | 2010 - 2019 |
|    369 | Ukraine                  | 2010 - 2019 |
|    370 | Belarus (Byelorussia)    | 2010 - 2019 |
|    371 | Armenia                  | 2010 - 2019 |
|    372 | Georgia                  | 2010 - 2019 |
|    373 | Azerbaijan               | 2010 - 2019 |
|    375 | Finland                  | 2010 - 2019 |
|    380 | Sweden                   | 2010 - 2019 |
|    385 | Norway                   | 2010 - 2019 |
|    390 | Denmark                  | 2010 - 2019 |
|    395 | Iceland                  | 2010 - 2019 |
|    402 | Cape Verde               | 2010 - 2019 |
|    404 | Guinea-Bissau            | 2010 - 2019 |
|    411 | Equatorial Guinea        | 2010 - 2019 |
|    420 | Gambia                   | 2010 - 2019 |
|    432 | Mali                     | 2010 - 2019 |
|    433 | Senegal                  | 2010 - 2019 |
|    434 | Benin                    | 2010 - 2019 |
|    435 | Mauritania               | 2010 - 2019 |
|    436 | Niger                    | 2010 - 2019 |
|    437 | Cote D’Ivoire            | 2010 - 2019 |
|    438 | Guinea                   | 2010 - 2019 |
|    439 | Burkina Faso             | 2010 - 2019 |
|    450 | Liberia                  | 2010 - 2019 |
|    451 | Sierra Leone             | 2010 - 2019 |
|    452 | Ghana                    | 2010 - 2019 |
|    461 | Togo                     | 2010 - 2019 |
|    471 | Cameroon                 | 2010 - 2019 |
|    475 | Nigeria                  | 2010 - 2019 |
|    481 | Gabon                    | 2010 - 2019 |
|    482 | CAR                      | 2010 - 2019 |
|    483 | Chad                     | 2010 - 2019 |
|    484 | Congo                    | 2010 - 2019 |
|    490 | DR Congo                 | 2010 - 2019 |
|    500 | Uganda                   | 2010 - 2019 |
|    501 | Kenya                    | 2010 - 2019 |
|    510 | Tanzania                 | 2010 - 2019 |
|    516 | Burundi                  | 2010 - 2019 |
|    517 | Rwanda                   | 2010 - 2019 |
|    520 | Somalia                  | 2012 - 2019 |
|    522 | Djibouti                 | 2010 - 2019 |
|    530 | Ethiopia                 | 2010 - 2019 |
|    531 | Eritrea                  | 2010 - 2019 |
|    540 | Angola                   | 2010 - 2019 |
|    541 | Mozambique               | 2010 - 2019 |
|    551 | Zambia                   | 2010 - 2019 |
|    552 | Zimbabwe (Rhodesia)      | 2010 - 2019 |
|    553 | Malawi                   | 2010 - 2019 |
|    560 | South Africa             | 2010 - 2019 |
|    565 | Namibia                  | 2010 - 2019 |
|    570 | Lesotho                  | 2010 - 2019 |
|    571 | Botswana                 | 2010 - 2019 |
|    572 | Swaziland                | 2010 - 2019 |
|    580 | Madagascar               | 2010 - 2019 |
|    581 | Comoros                  | 2010 - 2019 |
|    590 | Mauritius                | 2010 - 2019 |
|    600 | Morocco                  | 2010 - 2019 |
|    615 | Algeria                  | 2010 - 2019 |
|    616 | Tunisia                  | 2010 - 2019 |
|    620 | Libya                    | 2010 - 2019 |
|    625 | Sudan                    | 2010 - 2019 |
|    626 | South Sudan              | 2011 - 2019 |
|    630 | Iran                     | 2010 - 2019 |
|    640 | Turkey                   | 2010 - 2019 |
|    645 | Iraq                     | 2010 - 2019 |
|    651 | Egypt                    | 2010 - 2019 |
|    652 | Syria                    | 2010 - 2019 |
|    660 | Lebanon                  | 2010 - 2019 |
|    663 | Jordan                   | 2010 - 2019 |
|    666 | Israel                   | 2010 - 2019 |
|    670 | Saudi Arabia             | 2010 - 2019 |
|    678 | Yemen                    | 2010 - 2019 |
|    690 | Kuwait                   | 2010 - 2019 |
|    692 | Bahrain                  | 2010 - 2019 |
|    694 | Qatar                    | 2010 - 2019 |
|    696 | United Arab Emirates     | 2010 - 2019 |
|    698 | Oman                     | 2010 - 2019 |
|    700 | Afghanistan              | 2010 - 2019 |
|    701 | Turkmenistan             | 2010 - 2019 |
|    702 | Tajikistan               | 2010 - 2019 |
|    703 | Kyrgyz Republic          | 2010 - 2019 |
|    704 | Uzbekistan               | 2010 - 2019 |
|    705 | Kazakhstan               | 2010 - 2019 |
|    710 | China                    | 2010 - 2019 |
|    712 | Mongolia                 | 2010 - 2019 |
|    731 | North Korea              | 2010 - 2019 |
|    732 | South Korea              | 2010 - 2019 |
|    740 | Japan                    | 2010 - 2019 |
|    750 | India                    | 2010 - 2019 |
|    760 | Bhutan                   | 2010 - 2019 |
|    770 | Pakistan                 | 2010 - 2019 |
|    771 | Bangladesh               | 2010 - 2019 |
|    775 | Myanmar                  | 2010 - 2019 |
|    780 | Sri Lanka                | 2010 - 2019 |
|    781 | Maldives                 | 2010 - 2019 |
|    790 | Nepal                    | 2010 - 2019 |
|    800 | Thailand                 | 2010 - 2019 |
|    811 | Cambodia (Kampuchea)     | 2010 - 2019 |
|    812 | Laos                     | 2010 - 2019 |
|    816 | Vietnam                  | 2010 - 2019 |
|    820 | Malaysia                 | 2010 - 2019 |
|    830 | Singapore                | 2010 - 2018 |
|    840 | Philippines              | 2010 - 2019 |
|    850 | Indonesia                | 2010 - 2019 |
|    860 | East Timor               | 2010 - 2019 |
|    900 | Australia                | 2010 - 2019 |
|    910 | Papua New Guinea         | 2010 - 2019 |
|    920 | New Zealand              | 2010 - 2019 |
|    940 | Solomon Islands          | 2010 - 2019 |
|    950 | Fiji                     | 2010 - 2019 |

Countries covered by the test and live forecasts

``` r
write_csv(covered, "output/forecast-covered-countries.csv")

# Which G&W countries are *not* covered by the forecasts
not_covered <- anti_join(gw_full, forecast_data, by = c("gwcode", "year")) %>%
  # ID consecutive year sequences
  group_by(gwcode) %>%
  arrange(gwcode, year) %>%
  mutate(spell = id_date_sequence(year)) %>%
  # collapse over consecutive years
  group_by(gwcode, spell) %>%
  summarize(years = format_years(year)) %>%
  # add country names
  mutate(country = country_names(gwcode, shorten = TRUE)) %>%
  select(gwcode, country, years)
not_covered %>%
  knitr::kable(caption = "Countries *not* covered by the test and live forecasts")
```

| gwcode | country               | years       |
| -----: | :-------------------- | :---------- |
|     31 | Bahamas               | 2010 - 2019 |
|     54 | Dominica              | 2010 - 2019 |
|     55 | Grenada               | 2010 - 2019 |
|     56 | Saint Lucia           | 2010 - 2019 |
|     57 | Saint Vincent         | 2010 - 2019 |
|     58 | Antigua & Barbuda     | 2010 - 2019 |
|     60 | Saint Kitts and Nevis | 2010 - 2019 |
|     80 | Belize                | 2010 - 2019 |
|    221 | Monaco                | 2010 - 2019 |
|    223 | Liechtenstein         | 2010 - 2019 |
|    232 | Andorra               | 2010 - 2019 |
|    331 | San Marino            | 2010 - 2019 |
|    347 | Kosovo                | 2010 - 2019 |
|    396 | Abkhazia              | 2010 - 2019 |
|    397 | South Ossetia         | 2010 - 2019 |
|    403 | Sao Tome and Principe | 2010 - 2019 |
|    520 | Somalia               | 2010 - 2011 |
|    591 | Seychelles            | 2010 - 2019 |
|    713 | Taiwan                | 2010 - 2019 |
|    830 | Singapore             | 2019        |
|    835 | Brunei                | 2010 - 2019 |
|    935 | Vanuatu               | 2010 - 2019 |
|    970 | Kiribati              | 2010 - 2019 |
|    971 | Nauru                 | 2010 - 2019 |
|    972 | Tonga                 | 2010 - 2019 |
|    973 | Tuvalu                | 2010 - 2019 |
|    983 | Marshall Islands      | 2010 - 2019 |
|    986 | Palau                 | 2010 - 2019 |
|    987 | Micronesia            | 2010 - 2019 |
|    990 | Samoa/Western Samoa   | 2010 - 2019 |

Countries *not* covered by the test and live forecasts

``` r
write_csv(not_covered, "output/forecast-not-covered-countries.csv")
```

## Save

``` r
states_clean <- states[!drop_idx, ]
write_rds(states_clean, "output/states.rds")
```
