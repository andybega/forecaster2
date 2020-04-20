#
#   Extract variable importance from models
#

library(readr)
library(dplyr)
library(ggplot2)
library(here)

setwd(here::here("models"))

var_imp <- read_csv("output/full-model/variable-importance.csv")

# rescale var imp to 0-1
var_imp <- var_imp %>%
  group_by(outcome) %>%
  mutate(value = value / max(value))

var_imp %>% 
  arrange(outcome, desc(value)) %>%
  group_by(outcome) %>% 
  slice(1:10)

var_imp %>%
  ggplot(aes(x = value, y = reorder(variable, value), color = outcome)) +
  geom_point()
