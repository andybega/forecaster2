#
#   Keep track of successive forecast scores
#   Part of this is to get an idea of the performance variance
#


library(dplyr)
library(yardstick)
library(readr)

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
    N = n(),
    cases = sum(as.integer(as.character(observed))),
    pos_rate = mean(as.integer(as.character(observed))),
    roc_auc = safe_auc_roc(observed, p),
    roc_pr = safe_auc_pr(observed, p)
  )

old_acc <- read_csv("output/acc-archive.csv",
                    col_types = cols(
                      set_idx = col_double(),
                      date = col_date(format = ""),
                      outcome = col_character(),
                      N = col_integer(),
                      cases = col_integer(),
                      pos_rate = col_double(),
                      roc_auc = col_double(),
                      roc_pr = col_double()
                    ))

total_acc$set_idx <- max(old_acc$set_idx) + 1L
total_acc$date    <- lubridate::today()
total_acc <- total_acc %>% select(set_idx, date, everything())

acc <- bind_rows(old_acc, total_acc) 
write_csv(acc, "output/acc-archive.csv")
