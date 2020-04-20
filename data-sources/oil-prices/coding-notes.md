Oil prices
================
Andreas Beger
14 December 2018

Check here for updated data: <http://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy/downloads.html>

``` r
ff <- tail(dir("input", full.names = TRUE), 1)

sheets  <- excel_sheets(ff)
sheet_n <- which(grepl("Oil \\- Crude prices since", sheets))

oil_raw <- readxl::read_excel(ff, sheet = sheet_n, skip = 3)
oil_raw[, 4:7] <- NULL
oil_raw <- oil_raw %>%
  dplyr::filter(complete.cases(.)) 
colnames(oil_raw)[3] <- "oil_price"
oil_raw <- oil_raw[, c("Year", "oil_price")]

# Carry forward last year prices
# Since we lag in a second, don't need to tack on NA for last year
extrapolate_oil_prices <- function(oil, end_year = 2018) {
  oil <- merge(data.frame(Year = seq(min(oil_raw$Year), end_year - 1)),
               oil_raw, by = "Year", all.x = TRUE)
  oil <- zoo::na.locf(oil)
  oil
}
#oil <- extrapolate_oil_prices(oil_raw, 2018)
# don't extrapolate
oil <- oil_raw
oil$Year <- as.integer(oil$Year)
colnames(oil)[1] <- "year"
```

Supplement latest year with EIA oil prices
------------------------------------------

``` r
cushing <- rio::import("https://www.eia.gov/dnav/pet/hist_xls/RWTCd.xls",
                       sheet = 2, skip = 2)
colnames(cushing) <- c("date", "cushing")
brent   <- rio::import("https://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls",
                       sheet = 2, skip = 2)
colnames(brent) <- c("date", "brent")
eia <- full_join(cushing, brent, by = "date") %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  filter(date > "1987-12-31")

eia %>%
  tidyr::gather(var, value, -date) %>%
  ggplot(., aes(x = date, y = value, colour = var)) +
  geom_step()
```

![](coding-notes_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
# Annualize and take only last year
eia_yearly <- eia %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  dplyr::summarize(cushing = mean(cushing, na.rm=T),
                   brent = mean(brent, na.rm=T),
                   avg = (cushing + brent) / 2)

with(subset(oil, year > 2000),
     plot(year, oil_price, type = "l", lty = 1))
with(subset(as.data.frame(eia_yearly), year > 2000 & year < 2017),
     lines(year, avg, lty = 2))
with(subset(as.data.frame(eia_yearly), year > 2000 & year < 2017),
     lines(year, brent, lty = 3, col = "blue"))
with(subset(as.data.frame(eia_yearly), year > 2000 & year < 2017),
     lines(year, cushing, lty = 4, col = "red"))
```

![](coding-notes_files/figure-markdown_github/unnamed-chunk-2-2.png)

Conclusion: use brent for estimating current year oil prices.

``` r
# Estimate current oil prices
to_merge <- eia_yearly %>%
  mutate(oil_price = brent) %>%
  select(year, oil_price) %>%
  filter(year > max(oil$year))
oil <- bind_rows(oil, to_merge)


oil$oil_price_d1 <- c(NA, diff(oil$oil_price))

oil %>%
  tidyr::gather(var, value, -year) %>%
  ggplot(., aes(x = year, y = value)) +
  geom_line() + facet_wrap(~ var, scales = "free_y", ncol = 1) +
  theme_minimal()
```

![](coding-notes_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
meta <- list(
  dataset = "bp-oil",
  start   = as.integer(min(oil_raw$Year)),
  end     = as.integer(max(oil_raw$Year))
)

write_csv(oil, "output/oil-prices.csv")
```

Output data
-----------

|  year|  oil\_price|  oil\_price\_d1|
|-----:|-----------:|---------------:|
|  1861|       13.04|              NA|
|  1862|       25.14|           12.10|
|  1863|       61.15|           36.01|
|  1864|      123.18|           62.03|
|  1865|      102.91|          -20.28|
|  1866|       61.06|          -41.85|

    ## [1] 1861 2018
