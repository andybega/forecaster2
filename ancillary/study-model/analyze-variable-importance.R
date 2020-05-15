#
#   Extract variable importance from models
#

library(readr)
library(dplyr)
library(ggplot2)
library(here)
library(stringr)

setwd(here::here("models"))

var_imp <- read_csv("output/full-model/variable-importance.csv")

# rescale var imp to 0-1
var_imp <- var_imp %>%
  group_by(outcome) %>%
  mutate(value = value / max(value))

top10 <- var_imp %>% 
  arrange(outcome, desc(value)) %>%
  group_by(outcome) %>% 
  slice(1:10)
top10

# Save a summary of the top vars for git
top10 %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  write_csv("output/var-importance-top10.csv")

var_imp %>%
  ggplot(aes(x = value, y = reorder(variable, value), color = outcome)) +
  geom_point()


# Can some columns be discarded for modeling? -----------------------------
#
#   With 258 columns, the models take a looong time to run. Cane some of these
#   columns be dropped without a hit on accuracy?
#

# Is it worth keeping the _imputed variables around? They are 0/1 vectors, and 
# mostly 0 values in any case. 
var_imp %>%
  filter(str_detect(variable, "_imputed")) -> var_imp_imputed
# That's 147/3 = 49 columns

range(var_imp_imputed$value) %>% round(., 3)
# [1] 0.000 0.037

mean(var_imp_imputed$value)
# 0.0005

# Ok, not worth keeping around. Can be dropped prior to modeling.

# What about the year to year diff versions?
var_imp %>%
  filter(str_detect(variable, "(\\_diff)|(d1)")) -> var_imp_diff
# 150/3 = 50 columns

range(var_imp_diff$value) %>% round(., 3)
# [1] -0.003  0.043

mean(var_imp_diff$value)
# 0.006

# Also not much to look at. 

# _sd variables
var_imp %>%
  filter(str_detect(variable, "_sd")) -> var_imp_sd
# 138/3 = 46 columns

range(var_imp_sd$value) %>% round(., 3)
# [1] -0.002  0.106

mean(var_imp_sd$value)
# [1] 0.01273376

# Also can be taken out.  


# Reduced data variable importance ----------------------------------------
#
#   Take out the variables ID's above as not very important. 
#

var_imp_short <- var_imp %>%
  filter(!str_detect(variable, "_imputed"),
         !str_detect(variable, "(\\_diff)|(d1)"),
         !str_detect(variable, "_sd")) 

var_imp_short %>%
  ggplot(aes(x = value, y = reorder(variable, value), color = outcome)) +
  geom_point()

# Variable importance by data source and outcome
vi_by_source <- var_imp_short %>%
  mutate(source = str_extract(variable, "^[a-z]+\\_")) %>%
  group_by(outcome, source) %>%
  summarize(mean_value = mean(value),
            median_value = median(value),
            n_vars = n(),
            max_value = max(value))

# Variable importance by data source
vi_by_source_only <- vi_by_source %>%
  group_by(source) %>%
  summarize(n_vars = unique(n_vars),
            mean_value = mean(mean_value),
            max_value = max(max_value)) %>%
  arrange(desc(mean_value))
vi_by_source_only

# 2020-05-14
#
# A tibble: 8 x 4
#   source n_vars mean_value max_value
#   <chr>   <int>      <dbl>     <dbl>
# 1 wdi_        4    0.309      0.728 
# 2 years_      3    0.204      0.482 
# 3 vdem_      46    0.144      1     
# 4 ln_         1    0.0556     0.0609
# 5 pt_        15    0.0362     0.172 
# 6 oil_        1    0.0190     0.0380
# 7 reign_     33    0.0109     0.158 
# 8 epr_       10    0.00334    0.0191
#
# WDI (infmort and cell/internet) is the most miportant source, as well as the
# years since last coup indicators. EPR, REIGN, oil prices and the other P&T
# coup derived vars don't contribute much. 
#
var_imp_short %>%
  filter(str_detect(variable, "^wdi")) %>%
  arrange(outcome, desc(value))
# # A tibble: 12 x 3
# # Groups:   outcome [3]
#   variable                value outcome
#   <chr>                   <dbl> <chr>  
# 1 wdi_infmort            0.570  attempt
# 2 wdi_infmort_yearadj    0.431  attempt
# 3 wdi_internet_users_pct 0.0601 attempt
# 4 wdi_cellphones_per100  0.0480 attempt
# 5 wdi_infmort            0.675  coup   
# 6 wdi_infmort_yearadj    0.547  coup   
# 7 wdi_cellphones_per100  0.0295 coup   
# 8 wdi_internet_users_pct 0.0262 coup   
# 9 wdi_infmort            0.728  failed 
# 10 wdi_infmort_yearadj    0.500  failed 
# 11 wdi_internet_users_pct 0.0599 failed 
# 12 wdi_cellphones_per100  0.0280 failed 
#
# Infant mortality is by far the most useful WDI variable here. 
#
