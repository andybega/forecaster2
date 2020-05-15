Forecast models
===============

To run the actual forecast models:

```r
# Run the main forecast models for P(coup), P(failed coup attempt), P(any attempt)
source("run-forecasts.R")

# Run the bottom-up P(any attempt) version created by aggregating the 
# model-basedP(coup) and P(failed coup attempt) from above; this is the 
# version that is shown on the website
source("add-bottom-up-attempt.R")
```

There is an alternative, bottom-up, version of P(any attempt) that is created by aggregating up the P(successful coup) and P(failed coup attempt) forecasts. This is done by `add-bottom-up-attempt.R`, which is called in the `run-forecasts.R` script. 

Note that the model runner script will by default run in parallel using all as many workers as there are cores. 

The forecasts will be in `output/fcasts.rds`. 

Note that for tuning experiments and variable importance, there is supporting model in [/ancillary/study-model](/ancillary/study-model). The study model uses all available data and is run over a larger number of hyperparameter samples, with more extensive repeated CV. It's main purposes are to restrict the range of hyperparameter values that the actual forecast models are tuned over, and to provide some estimates of variable importance. It's not really needed to generate the forecasts. 

## Score archive

From run 1 to 8, an error which introduced an extra attempt case (1508 vs 1507) was eliminated.

Run 8, updated EPR data were included which expanded the case set

Run 9 is identical to 8. 

Run 10 adds fully imputed V-Dem data with bigger case set as result, additional V-Dem and EPR variable transformations. 

Run 11 adds WDI ICT variables. 

