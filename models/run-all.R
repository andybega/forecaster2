#
#   Run both the forecast and study models
#

library(here)

setwd(here::here("models"))

source("run-study-model.R", echo = TRUE)
source("run-forecasts.R", echo = TRUE)
