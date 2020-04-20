Forecast models
===============


## Score archive

From run 1 to 8, an error which introduced an extra attempt case (1508 vs 1507) was eliminated.

From run 7 to 8, updated EPR data were included which expanded the case set


```r
acc <- read_csv("output/acc-archive.csv")

ggplot(acc, aes(x = roc_auc, y = roc_pr, color = outcome)) + geom_point()

acc %>%
  pivot_longer(roc_auc:roc_pr, names_to = "stat") %>%
  ggplot(aes(x = set_idx, y = value, color = outcome)) + 
  facet_wrap(~ stat) + 
  geom_point()
  
ggplot(acc, aes(x = roc_auc, y = roc_pr, color = outcome)) + geom_point()
```