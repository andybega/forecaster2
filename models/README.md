Forecast models
===============


## Score archive

From run 1 to 8, an error which introduced an extra attempt case (1508 vs 1507) was eliminated.

Run 8, updated EPR data were included which expanded the case set

Run 9 is identical to 8. 

Run 10 adds fully imputed V-Dem data with bigger case set as result, additional V-Dem and EPR variable transformations. 

Run 11 adds WDI ICT variables. 

```r
acc <- read_csv("output/acc-archive.csv")

ggplot(acc, aes(x = roc_auc, y = roc_pr, color = outcome)) + geom_point()

acc %>%
  pivot_longer(roc_auc:roc_pr, names_to = "stat") %>%
  ggplot(aes(x = set_idx, y = value, color = outcome)) + 
  facet_wrap(~ stat, scales = "free_y") + 
  geom_point()
  
ggplot(acc, aes(x = roc_auc, y = roc_pr, color = outcome)) + geom_point()
```