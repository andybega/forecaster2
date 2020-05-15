#
#   Analyze tuning results
#

library(ggplot2)
library(dplyr)
library(tidyr)
library(gstat)
library(earth)
library(tidyr)
library(viridis)
library(readr)
library(purrr)
library(here)

setwd(here::here("models"))

#
#   Full study model ----
#   __________________________

tr <- read_csv("output/full-model/tuning-results.csv")

p <- tr %>%
  pivot_longer(num.trees:min.node.size) %>%
  ggplot(., aes(x = value, y = classif.auc)) +
  facet_wrap(~task_id + name, scales = "free") +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Study model tuning results")
p
ggsave("output/figures/tune-results.png", plot = p, height = 8, width = 8)


#   Contour plot ----
#   _____________
#
#   I want to show two hyperparameters on the x and y axes, and then an 
#   estimate of the associated cost surface. There are several considerations:
#   
#   1. I start with an irregular grid, i.e. it's not a uniform sample.
#   2. A given point may have multiple cost samples
#   3. For ggplot2::geom_contour to work, I need a grid with single value
#      at each point. Or I might as well just do a tile. 
#
#   

par_grid <- crossing(mtry = seq(min(tr$mtry), max(tr$mtry), by = 1), 
                     min.node.size = seq(min(tr$min.node.size), max(tr$min.node.size), by = 10))

# Fit the interpolation models
interp_models <- tr %>%
  group_by(task_id) %>%
  nest() %>%
  mutate(model = map(data, ~earth(classif.auc ~ mtry + min.node.size, data = .x)),
         grid = list(par_grid)) %>%
  mutate(grid = map2(model, grid, ~{.y$ecost = predict(.x, newdata = .y, type = "response")[, "classif.auc"]; .y}))

grid <- interp_models %>%
  select(task_id, grid) %>%
  unnest(grid)

# the problem with this grid as it is now is that the different outcomes
# have very different baseline performance rates. So plotting with the same
# color scale will not show differences well accross outcomes. Standardize the 
# performance measure.

# keep the normalization parameters so we can norm tr as well
norm_pars <- grid %>%
  group_by(task_id) %>%
  summarize(group_mean = mean(ecost),
            group_sd = sd(ecost))

grid <- grid %>%
  group_by(task_id) %>%
  mutate(ecost = (ecost - mean(ecost))/sd(ecost))

norm_tr <- tr %>%
  left_join(norm_pars) %>%
  mutate(norm_cost = (classif.auc - group_mean)/group_sd)

ggplot(grid, aes(x = mtry, y = min.node.size)) +
  facet_wrap(~ task_id) +
  geom_tile(aes(fill = ecost), alpha = 0.5) +
  scale_fill_viridis("AUC-ROC, std", direction = 1) +
  geom_contour(aes(z = ecost), color = "gray50", alpha = 0.5) +
  theme_minimal() +
  geom_point(data = norm_tr, aes(color = norm_cost)) +
  scale_color_viridis(guide = FALSE)

ggsave("output/figures/tune-results-contour.png", height = 4, width = 12)


#
#   Forecast models ----
#   _________________

tr <- read_csv("output/tuning-results.csv")

p <- tr %>%
  pivot_longer(num.trees:min.node.size) %>%
  ggplot(., aes(x = value, y = classif.auc)) +
  facet_wrap(~task_id + name, scales = "free_x") +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Forecast models tuning results")
p
ggsave("output/figures/tune-results-fcast-models.png", plot = p, height = 8, width = 8)

