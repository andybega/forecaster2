
library(dplyr)
library(yardstick)

fcasts <- read_rds("output/fcasts.rds")

safe_auc_roc <- function(y, phat) {
  tryCatch({
    yardstick::roc_auc_vec(y, phat)
  }, error = function(e) NA_real_)
}

safe_auc_pr <- function(y, phat) {
  tryCatch({
    yardstick::pr_auc_vec(y, phat)
  }, error = function(e) NA_real_)
}

total_acc <- fcasts %>%
  filter(!is.na(observed)) %>%
  group_by(outcome) %>%
  summarize(
    cases = sum(as.integer(as.character(observed))),
    pos_rate = mean(as.integer(as.character(observed))),
    roc_auc = safe_auc_roc(observed, p),
    roc_pr = safe_auc_pr(observed, p)
  )

knitr::kable(total_acc, digits = 3) %>%
  writeLines("output/tbl-total-acc.md")

