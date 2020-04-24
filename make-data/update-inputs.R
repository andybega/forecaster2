#
#   Copy over input files from data-sources/
#

library(here)

oldwd = getwd()
setwd(here::here("make-data"))

file.copy(
  from = "../data-sources/v-dem/output/v-dem.csv",
  to = "input/v-dem.csv",
  overwrite = TRUE
)

file.copy(
  from = "../data-sources/wdi-infmort/output/wdi-infmort.csv",
  to = "input/wdi-infmort.csv",
  overwrite = TRUE
)

file.copy(
  from = "../data-sources/wdi-ict/output/wdi-ict.csv",
  to = "input/wdi-ict.csv",
  overwrite = TRUE
)

file.copy(
  from = "../data-sources/epr/output/epr.csv",
  to = "input/epr.csv",
  overwrite = TRUE
)

file.copy(
  from = "../data-sources/ptcoups/output/ptcoups.csv",
  to = "input/ptcoups.csv",
  overwrite = TRUE
)

file.copy(
  from = "../data-sources/oil-prices/output/oil-prices.csv",
  to = "input/oil-prices.csv",
  overwrite = TRUE
)

setwd(oldwd)