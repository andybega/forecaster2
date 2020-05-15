#
#   Run the test and live forecasts
#

set.seed(5232)

library(mlr3)
library(mlr3learners)
library(paradox)
requireNamespace("mlr3measures")
library(mlr3tuning)
library(here)
library(dplyr)
library(stringr)
library(lgr)
library(readr)
library(future)
library(doFuture)
library(ranger)
library(future.apply)
library(yaml)

lgr$info("Start forecast models")
t_start <- Sys.time()

setwd(here::here("models"))

registerDoFuture()
plan(multiprocess(workers = availableCores()))

states <- readRDS("input/states.rds") %>%
  as_tibble() 

# mlr3 needs DV to be factors
states <- states %>%
  mutate_at(.vars = vars(ends_with("lead1"), ends_with("lead2")),
            .funs = as.factor) 

non_feat_cols <- c("gwcode", "year", names(states)[str_detect(names(states), "lead[0-9]")])

tasks <- list(
  coup = TaskClassif$new(id = "coup", backend = states, 
                          target = "pt_coup_lead1", positive = "1"),
  failed = TaskClassif$new(id = "failed", backend = states, 
                           target = "pt_failed_lead1", positive = "1"),
  attempt = TaskClassif$new(id = "attempt", backend = states, 
                            target = "pt_attempt_lead1", positive = "1")
)

# mark non-feature columns as such so mlr doesn't use them to train models
tasks <- sapply(tasks, function(x) {
  x$col_roles$feature = setdiff(x$col_roles$feature, non_feat_cols)
  x
})

# How many feature columns?
n_feats = length(names(states)[!names(states) %in% non_feat_cols])

learner = mlr3::lrn(
  "classif.ranger", 
  predict_type = "prob",
  num.trees = 1000,
  min.node.size = 270,
  mtry = floor(sqrt(n_feats))
)

auto_rf = learner

# Construct the grid of models we will run
# For each year, 3 models for the 3 outcomes
model_grid <- expand.grid(year = 2010:2019, outcome = names(tasks), 
                          stringsAsFactors = FALSE)

# mlr will recognize and use the parallel backend for each model. So rather 
# than running [available workers] model_grid rows at the same time, each single
# model_grid row will end up being run in parallel.
for (i in 1:nrow(model_grid)) {
  
  t0 <- proc.time()
  yy <- model_grid$year[[i]]
  outcome_i <- model_grid$outcome[[i]]
  
  lgr$info("Year: %d, Outcome: %s", yy, outcome_i)
  
  task_i     <- tasks[[outcome_i]]
  fcast_i_fn <- sprintf("output/chunks/forecast/%s-%d.rds", outcome_i, yy)             
  tune_i_fn  <- sprintf("output/chunks/tuning/%s-%d.csv", outcome_i, yy)
  
  train_rows <- which(states$year < yy)
  pred_rows  <- which(states$year==yy)
  
  auto_rf$train(task_i, row_ids = train_rows)
  fcast_i <- auto_rf$predict(task_i, row_ids = pred_rows)
  
  out_i <- states[pred_rows, c("gwcode", "year")]
  out_i$for_year <- unique(out_i$year) + 1L
  out_i$outcome  <- outcome_i
  out_i$observed <- states[pred_rows, ][[task_i$col_roles$target]]
  out_i$p <- fcast_i$prob[, 1]
  
  write_rds(out_i, path = fcast_i_fn)
}


# Combine forecast chunks
chunk_files <- dir("output/chunks/forecast", full.names = TRUE)
chunks <- lapply(chunk_files, readr::read_rds)
fcasts <- bind_rows(chunks)

# Fix data types
fcasts <- fcasts %>%
  mutate(
    # "1" should be first level
    observed = forcats::fct_rev(observed),
    gwcode = as.integer(gwcode),
    year = as.integer(year),
    for_year = as.integer(for_year)
  )

stopifnot(levels(fcasts$observed)[1]=="1")

# for tests forecasts, maybe need custom resampling; or manually iterature through 
# years
# https://mlr3book.mlr-org.com/resampling.html

write_csv(fcasts, "output/fcast.csv")
# Write RDS also because CSV will have precision limits
write_rds(fcasts, "output/fcasts.rds")

## TPR vs FPR / Sensitivity vs (1 - Specificity)
#ggplot2::autoplot(pred, type = "roc")

warn <- warnings()
if (length(warn) > 1) {
  call <- as.character(warn)
  msg  <- names(warn)
  warn_strings <- paste0("In ", call, " : ", msg)
  lgr$warn("There were R warnings, printing below:")
  for (x in warn_strings) lgr$warn(x)
}

# Track summary stats for the data this was run with; I had trouble keeping
# track when I was concurrently running models and changing data
tbl <- list(
  date = as.character(Sys.Date()),
  N = nrow(states),
  N_in_forecast_sets = nrow(states[states$year %in% 2010:2019, ]),
  Years = paste0(range(states[["year"]]), collapse = " - "),
  Features = as.integer(ncol(states) - 2 - sum(str_detect(names(states), "lead[0-9]"))),
  Positive_attempt_lead1 = as.integer(
    sum(states[["pt_attempt_lead1"]]=="1", na.rm = TRUE)
  ),
  Positive_coup_lead1 = as.integer(
    sum(states[["pt_coup_lead1"]]=="1", na.rm = TRUE)
  ),
  Positive_failed_lead1 = as.integer(
    sum(states[["pt_failed_lead1"]]=="1", na.rm = TRUE)
  ),
  Time = as.numeric((Sys.time() - t_start))
) 

tbl %>%
  yaml::as.yaml() %>%
  writeLines("output/forecast-models-last-run.yml")

lgr$info("Forecast models finished, total time: %ss", round((proc.time() - t0)["elapsed"]))

source("score-archive.R")
source("score.R")
