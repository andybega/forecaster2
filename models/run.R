

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

setwd(here::here("models"))

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


learner         = mlr3::lrn("classif.ranger", predict_types = "prob")
learner$predict_type = "prob"
tune_resampling = rsmp("repeated_cv")
tune_resampling$param_set$values = list(repeats = 1, folds = 5)
tune_measures   = msr("classif.auc")
tune_ps = ParamSet$new(list(
  ParamInt$new("num.trees", lower = 500, upper = 3000),
  ParamInt$new("mtry", lower = 3, upper = 15),
  ParamInt$new("min.node.size", lower = 1, upper = 400)
))
tune_terminator = term("evals", n_evals = 2)
tuner = mlr3tuning::tnr("random_search")

auto_rf = AutoTuner$new(
  learner = learner,
  resampling = tune_resampling,
  measures = tune_measures,
  tune_ps = tune_ps,
  terminator = tune_terminator,
  tuner = tuner
)

# Iterate through test and live forecast years
years <- 2010:2019
fcasts <- list()
for (i in seq_along(years)) {
  yy <- years[i]
  
  train_rows <- which(states$year < yy)
  pred_rows  <- which(states$year==yy)
  
  # Iterate through the 3 different outcomes
  fcasts_yy <- list()
  for (outcome_i in names(tasks)) {
    
    lgr$info("Year: %d, Outcome: %s", yy, outcome_i)
    
    task_i <- tasks[[outcome_i]]
    
    auto_rf$train(task_i, row_ids = train_rows)
    fcast_i <- auto_rf$predict(task_i, row_ids = pred_rows)
    
    # This portion is related to optimizing the tuning process
    tr <- auto_rf$tuning_instance$archive(unnest = "params") %>%
      select(task_id, learner_id, resampling_id, iters, classif.auc, num.trees,
             mtry, min.node.size)
    fn <- sprintf("output/tuning/%s-%d.csv", outcome_i, yy)
    write_csv(tr, path = fn)
    ## End tuning section
    
    out_i <- states[pred_rows, c("gwcode", "year")]
    out_i$for_year <- unique(out_i$year) + 1L
    out_i$outcome  <- outcome_i
    out_i$observed <- states[pred_rows, ][[task_i$col_roles$target]]
    out_i$p <- fcast_i$prob[, 1]
    
    fcasts_yy[[outcome_i]] <- out_i
  }
  fcasts_yy <- bind_rows(fcasts_yy)
  fcasts[[i]] <- fcasts_yy
}
fcasts <- bind_rows(fcasts)

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

source("score.R")
