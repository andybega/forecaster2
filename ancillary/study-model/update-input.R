#
#   Copy over input files from data-sources/
#

library(here)

oldwd = getwd()
setwd(here::here("models"))

file.copy(
  from = "../make-data/output/states.rds",
  to = "input/states.rds",
  overwrite = TRUE
)

setwd(oldwd)
