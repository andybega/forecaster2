#
#   Run both the forecast and study models
#

library(here)

setwd(here::here("models"))

source("run-study-model.R")
source("run-forecasts.R")
