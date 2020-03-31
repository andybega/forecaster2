#
#   Run the test and live forecasts
#


library(mlr3)
library(mlr3learners)
library(paradox)
library(mlr3measures)
library(mlr3tuning)
library(here)
library(dplyr)
library(stringr)
library(lgr)
library(readr)
library(future)
library(doFuture)

lgr$info("Start forecast models")

setwd(here::here("models"))

registerDoFuture()
plan(multisession(workers = availableCores()))

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


learner         = mlr3::lrn("classif.ranger", 
                            predict_type = "prob",
                            num.trees = 1200,
                            mtry = 4)
tune_resampling = rsmp("repeated_cv")
tune_resampling$param_set$values = list(repeats = 2, folds = 8)
tune_measures   = msr("classif.auc")
tune_ps = ParamSet$new(list(
  ParamInt$new("min.node.size", lower = 1, upper = 200)
))
tune_terminator = term("evals", n_evals = 20)
tuner = mlr3tuning::tnr("random_search")

auto_rf = AutoTuner$new(
  learner = learner,
  resampling = tune_resampling,
  measures = tune_measures,
  tune_ps = tune_ps,
  terminator = tune_terminator,
  tuner = tuner
)

# Construct the grid of models we will run
# For each year, 3 models for the 3 outcomes
model_grid <- expand.grid(year = 2010:2019, outcome = names(tasks), 
                          stringsAsFactors = FALSE)

r <- foreach(
  i = 1:nrow(model_grid),
  .export = c("model_grid", "states", "auto_rf", "tasks", "non_feat_cols"),
  .packages = c("mlr3", "mlr3learners", "mlr3measures", "mlr3tuning", "paradox",
                "readr")
) %dopar% {
  
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
  
  # This portion is related to optimizing the tuning process
  tr <- auto_rf$tuning_instance$archive(unnest = "params") %>%
    select(task_id, learner_id, resampling_id, iters, classif.auc, num.trees,
           mtry, min.node.size)
  write_csv(tr, path = tune_i_fn)
  ## End tuning section
  
  out_i <- states[pred_rows, c("gwcode", "year")]
  out_i$for_year <- unique(out_i$year) + 1L
  out_i$outcome  <- outcome_i
  out_i$observed <- states[pred_rows, ][[task_i$col_roles$target]]
  out_i$p <- fcast_i$prob[, 1]
  
  write_rds(out_i, path = fcast_i_fn)
  
  invisible(NULL)
}

# Combine tune chunks
chunk_files <- dir("output/chunks/tuning", full.names = TRUE)
chunks <- lapply(chunk_files, readr::read_csv)
tr <- bind_rows(chunks)
write_csv(tr, "output/tuning-results.csv")

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

source("score.R")
