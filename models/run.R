

library(mlr3)
library(mlr3learners)
library(here)

setwd(here::here("models"))

states <- readRDS("input/states.rds") %>%
  as_tibble()

# mlr3 needs DV to be factors
states <- states %>%
  mutate_at(.vars = vars(ends_with("lead1"), ends_with("lead2")),
            .funs = as.factor) 

non_feat_cols <- c("gwcode", "year", names(states)[str_detect(names(states), "lead[0-9]")])

train_rows <- which(states$year < 2019)
pred_rows  <- which(states$year==2019)

task_coups <- TaskClassif$new(id = "coups", backend = states, 
                              target = "pt_coup_lead1", positive = "1")
task_coups$col_roles$feature = setdiff(task_coups$col_roles$feature, non_feat_cols)

coups_logreg <- mlr3::lrn("classif.log_reg", predict_type = "prob")

coups_logreg$train(task_coups, row_ids = train_rows)

fcast <- coups_logreg$predict(task_coups, row_ids = pred_rows)

# for tests forecasts, maybe need custom resampling; or manually iterature through 
# years
# https://mlr3book.mlr-org.com/resampling.html

out <- states[pred_rows, c("gwcode", "year", "pt_coup_lead1")]
out$for_year <- 2020L
out$outcome  <- "pt_coup"
out$observed <- out$pt_coup_lead1
out$pt_coup_lead1 <- NULL
out$p <- fcast$prob[, 1]

write_csv(out, "output/fcast.csv")

## TPR vs FPR / Sensitivity vs (1 - Specificity)
#ggplot2::autoplot(pred, type = "roc")
