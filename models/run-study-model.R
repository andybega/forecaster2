#
#   Run a model on full data to study tuning results and variable importance
#
#   This script only runs one set of models using all available data, but,
#   it runs a larger number of tuning samples. 
#

CV_FOLDS   = 16
CV_REPEATS = 2
TUNE_N     = 20

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

lgr$info("Start study models")

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
                            importance = "permutation")
tune_resampling = rsmp("repeated_cv")
tune_resampling$param_set$values = list(repeats = CV_REPEATS, folds = CV_FOLDS)
tune_measures   = msr("classif.auc")
tune_ps = ParamSet$new(list(
  ParamInt$new("num.trees", lower = 500, upper = 2000),
  ParamInt$new("mtry", lower = 1, upper = 15),
  ParamInt$new("min.node.size", lower = 1, upper = 400)
))
tune_terminator = term("evals", n_evals = TUNE_N)
tuner = mlr3tuning::tnr("random_search")

auto_rf = AutoTuner$new(
  learner = learner,
  resampling = tune_resampling,
  measures = tune_measures,
  tune_ps = tune_ps,
  terminator = tune_terminator,
  tuner = tuner
)

# mlr will recognize and use the parallel backen
# loop over the outcome
yy <- 2019
for (outcome_i in names(tasks)) {
  
  lgr$info("Outcome: %s", outcome_i)
  
  task_i   <- tasks[[outcome_i]]
  model_fn <- sprintf("output/full-model/model-.rds", outcome_i)
  tune_fn  <- sprintf("output/full-model/tuning-results-%s.csv", outcome_i)
  imp_fn   <- sprintf("output/full-model/variable-importance-%s.csv", outcome_i)
  
  # the last year of data has missing values for the outcome, don't use
  train_rows <- which(states$year < yy)
  
  auto_rf$train(task_i, row_ids = train_rows)
  
  # Save the full model
  write_rds(auto_rf, model_fn)
  
  # This portion is related to optimizing the tuning process
  tr <- auto_rf$tuning_instance$archive(unnest = "params") %>%
    select(task_id, learner_id, resampling_id, iters, classif.auc, num.trees,
           mtry, min.node.size)
  write_csv(tr, path = tune_fn)
  ## End tuning section
  
  # Extract feature importance
  imp <- auto_rf$learner$importance() %>% 
    enframe(name = "variable") %>% arrange(desc(value))
  write_csv(imp, path = imp_fn)
  
}

# Combine tune chunks
chunk_files <- dir("output/full-model", pattern = "tuning\\-results\\-[a-z]+.csv", full.names = TRUE)
chunks <- lapply(chunk_files, readr::read_csv)
tr <- bind_rows(chunks)
write_csv(tr, "output/full-model/tuning-results.csv")

# Combine importance chunks
chunk_files <- dir("output/full-model", pattern = "variable\\-importance\\-[a-z]+.csv", full.names = TRUE)
chunks <- lapply(chunk_files, readr::read_csv)
imp <- bind_rows(chunks)
write_csv(imp, "output/full-model/variable-importance.csv")

lgr$info("Study models finished")

source("../analyse-tune-results.R")
