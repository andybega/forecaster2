#
#   Copy over input files from data-sources/
#

library(here)

oldwd = getwd()
setwd(here::here("make-data"))

file.copy(
  from = "../data-sources/v-dem/output/v-dem.csv",
  to = "input/v-dem.csv"
)

setwd(oldwd)