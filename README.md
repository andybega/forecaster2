Coup forecasts
==============

mini TODO:

- add updated ptcoups (with new counter), oil prices, EPR vars to states and re-run models
- V-Dem cautiously add v2x[a-z]{2} indicators if they don't add missing cases
- add WDI ICT to data

TODO:

- add sovereign defaults as outcome
- add data source modules for gw-state-age, reign-cy
- for V-Dem:
  + add "v2x[a-z]{2}" form indices
  + this is a fucking nightmare; impute missing values as they currently are
    before adding var transforms without missing values; then add more indices
- add sources:
  - [x] V-Dem
  - [x] Inf mort
  - [x] WDI cell phones
  - [x] oil prices
  - UCDP ACD
  - GDP, pop
  - FP.CPI.TOTL.ZG
  - Freedom House
  - Polity
  - FAO fpi
  
  
Variable importance from 2018 forecasts:

```
## SP.DYN.IMRT.IN         100.000
## v2x_liberal             54.037
## pt_attempt_num10yrs     50.538
## pt_attempt_num5yrs      30.590
## v2x_egal                20.783
## IT.CEL.SETS.P2          16.328
## v2x_api                 13.545
## oil_price               12.811
## NY.GDP.MKTP.KD          10.827
## fpi_real                 9.796
## v2x_libdem               9.128
## v2x_partipdem            8.749
## epr_excluded_group_pop   8.216
## v2x_delibdem             5.748
## FP.CPI.TOTL.ZG           5.052
## SP.POP.TOTL              4.839
## epr_inpower_groups_pop   3.451
## NY.GDP.DEFL.KD.ZG        2.666
## v2x_partip               2.626
## v2xdl_delib              2.353
```

