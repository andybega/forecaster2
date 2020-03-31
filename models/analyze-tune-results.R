#
#   Analyze tuning results
#

library(ggplot2)
library(dplyr)

tr <- read_csv("output/full-model/tuning-results.csv")

p <- tr %>%
  pivot_longer(num.trees:min.node.size) %>%
  ggplot(., aes(x = value, y = classif.auc)) +
  facet_wrap(~task_id + name, scales = "free") +
  geom_point() +
  geom_smooth() +
  theme_minimal()
p
ggsave("output/figures/tune-results.png", plot = p, height = 8, width = 8)

tr <- read_csv("output/tuning-results.csv")

p <- tr %>%
  pivot_longer(num.trees:min.node.size) %>%
  ggplot(., aes(x = value, y = classif.auc)) +
  facet_wrap(~task_id + name, scales = "free") +
  geom_point() +
  geom_smooth() +
  theme_minimal()
p
ggsave("output/figures/tune-results-fcast-models.png", plot = p, height = 8, width = 8)