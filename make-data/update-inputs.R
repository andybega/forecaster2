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

file.copy(
  from = "../data-sources/wdi-infmort/output/wdi-infmort.csv",
  to = "input/wdi-infmort.csv"
)

setwd(oldwd)