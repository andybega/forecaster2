#
#   Update inputs coming from other folders
#

setwd(here::here("website/_data"))

file.copy("../../models/output/fcasts.rds", "fcasts.rds", overwrite = TRUE)

