# 
#   Simulate the distribution of events expected under the test forecasts
#   in order to check how they map onto actual events. This is specifically
#   to see if the models pick up on the global decline in the frequence of 
#   coups.
#

library(here)
library(tidyverse)
library(ggridges)

setwd(here::here("models"))

#' Expected number of events given a vector of probabilities for a binary
#' outcome. 
sim_binom <- function(p, n = 1e3) {
  Np <- length(p)
  sims <- replicate(n, sum(rbinom(Np, 1, p)), simplify = "vector")
  sims
}

sim_pdf <- function(p, n) {
  sims <- sim_binom(p, n)
  pdf  <- tibble(yhat = sims) %>%
    group_by(yhat) %>%
    count()
}

sim_pdf_wrapper <- function(x, y, n) {
  sims <- sim_pdf(x, n)
  crossing(y, sims)
}

fcasts <- read_rds("output/fcasts.rds")

sims <- fcasts %>%
  group_by(for_year, outcome) %>%
  group_map(~sim_pdf_wrapper(.x$p, .y, n = 1e3)) %>%
  bind_rows() 

sims <- sims %>%
  rename(year = for_year) %>%
  group_by(year, outcome) %>%
  mutate(n = n/sum(n))

observed <- fcasts %>%
  group_by(for_year, outcome) %>%
  summarize(observed = sum(observed=="1")) %>%
  ungroup() %>%
  rename(year = for_year,
         yhat = observed)

expected <- sims %>% 
  group_by(year, outcome) %>% 
  summarize(yhat = sum(yhat * n))

p <- ggplot(sims, aes(x = yhat, y = factor(year))) +
  facet_wrap(~ outcome) + 
  geom_ridgeline(aes(height = n), stat = "identity", scale = 3) +
  geom_point(data = observed) +
  geom_point(data = expected, shape = 2, color = "red") + 
  theme_minimal() +
  labs(x = "Expected number of events", y = "Year")

ggsave(plot = p, filename = "output/figures/expected-no-events.png", height = 5, width = 8)

# How much probability was put on the observed count?
obs_bin <- sims %>%
  left_join(observed %>% mutate(observed = 1)) %>%
  filter(!is.na(observed))

ggplot(obs_bin, aes(x = year, y = n, color = outcome)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)



