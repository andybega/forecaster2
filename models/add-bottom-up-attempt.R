#
#   Add a bottom-up attempt forecast ("attempt2") that is created by aggregating
#   the model-driven coup and failed forecasts. 
#
#   One reason to do this is that the attempt forecasts are not always
#   greater than the failed or coup forecasts, even though logically 
#   they should be. 
#

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

setwd(here::here("models"))

fcasts <- read_rds("output/fcasts.rds")

# If attempt2 is already present, filter it out; this script will recreate it.
if ("attempt2" %in% unique(fcasts$outcome)) {
  fcasts <- fcasts %>%
    filter(outcome!="attempt2")
}

# How often are the attempt forecasts greater than the coup or failed forecasts?
fcasts %>%
  # observed depends on the outcome and will not be unique by row when we 
  # pivot the predictions to columns below; this will lead to NA's
  select(-observed) %>%
  pivot_wider(names_from = outcome, values_from = p) %>%
  summarize(
    attempt_gt_coup   = mean(attempt >= coup),
    attempt_gt_failed = mean(attempt >= failed)
  )

#   attempt_gt_coup attempt_gt_failed
#             <dbl>             <dbl>
# 1           0.850             0.889

# Mhm. The attempt forecasts are greater than the coup and failed forecasts 
# only ~80% of the time. 

fcasts <- fcasts %>% 
  arrange(outcome, gwcode, year)

# Create the bottom-up aggregation
attempt2 <- fcasts %>%
  filter(outcome %in% c("coup", "failed")) %>%
  group_by(gwcode, year, for_year) %>%
  dplyr::summarize(outcome = "attempt2", 
            observed = factor(max(observed=="1"), levels = c("1", "0")),
            p = sum(p) - prod(p), 
            n = n()) %>%
  ungroup()

if (any(attempt2$n != 2)) {
  stop("Something is wrong, there should only be 2 probs per groups")
}

attempt2 <- attempt2 %>%
  select(-n) %>%
  select(gwcode, year, for_year, outcome, observed, p)

fcasts <- fcasts %>%
  bind_rows(attempt2)

# Make sure the truth data is correct
fcasts %>% dplyr::filter(outcome=="attempt") %>% dplyr::count(observed)
fcasts %>% dplyr::filter(outcome=="attempt2") %>% dplyr::count(observed)

# Compare accuracy of attempt and attemp2
fcasts %>%
  group_by(outcome) %>%
  dplyr::summarize(AUC_ROC = roc_auc_vec(observed, p),
            AUC_PR  = pr_auc_vec(observed, p),
            pos_rate = mean(observed=="1", na.rm = TRUE),
            mean_p   = mean(p, na.rm = TRUE))

#   outcome  AUC_ROC AUC_PR pos_rate mean_p
#   <chr>      <dbl>  <dbl>    <dbl>  <dbl>
# 1 attempt    0.794 0.0479  0.0119  0.0245
# 2 attempt2   0.810 0.0474  0.0119  0.0279
# 3 coup       0.781 0.0435  0.00659 0.0139
# 4 failed     0.800 0.0209  0.00593 0.0145

# The bottom-up probs tend to be higher; all predictions generally 
# tend to be too high. 
fcasts %>%
  filter(outcome %in% c("attempt", "attempt2")) %>%
  pivot_wider(names_from = outcome, values_from = p) %>%
  ggplot(aes(x = attempt2, y = attempt)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") + 
  theme_minimal()

# Make sure attempt2 is always greater than coup or failed
check <- fcasts %>%
  # observed depends on the outcome and will not be unique by row when we 
  # pivot the predictions to columns below; this will lead to NA's
  select(-observed) %>%
  filter(outcome!="attempt") %>%
  pivot_wider(names_from = outcome, values_from = p) %>%
  summarize(
    attempt2_gt_coup   = mean(attempt2 >= coup),
    attempt2_gt_failed = mean(attempt2 >= failed)
  )

if (any(unlist(check) < 1)) {
  stop("attempt2 is not always greater than coup or failed, something is wrong")
}

# Save the extended forecasts
fcasts %>%
  arrange(year, gwcode) %>%
  write_rds(., "output/fcasts.rds")

