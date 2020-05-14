Make the combined data
======================

This folder pulls together various data sources cleaned and ingested in `data-sources` to create `output/states.rds`, which will be used as training and testing data for the forecast models. 

The heavy lifting is done by `combine-data.Rmd`, whose output you can see in [combine-data.md](combine-data.md). 

Aside from the main output, `output/states.rds`, there are several ancillary files that summarize the dataset, what countries are covered and not covered, and so on. I set these up so that it is easier to track changes in the data as I added new data sources or imputed more missing values in a data source. The file [`output/incomplete-case.csv`](output/incomplete-cases.csv) by the way lists all country years, using the Gleditsch & Ward state list to define relevant country-years, that are *incomplete* and thus taken out prior to modeling, along with the data source(s) that are responsible for the missing values. 
