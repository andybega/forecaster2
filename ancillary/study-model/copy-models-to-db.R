#
#   Copy the large study model saved models to DB
#

library(here)

setwd(here::here())

db_path <- "D:\\Dropbox/Work/forecaster2"

model_files <- dir("models/output/full-model", pattern = "model[-a-z]+\\.rds",
                   full.names = TRUE)

for (i in model_files) {
  file.copy(i, to = file.path(db_path, i), overwrite = TRUE)
}
