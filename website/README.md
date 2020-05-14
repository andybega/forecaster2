Coup forecasts website
======================

The website is built using [**rmarkdown**'s built-in site generator](https://bookdown.org/yihui/rmarkdown/rmarkdown-site.html) and uses [**flexdashboard**](https://rmarkdown.rstudio.com/flexdashboard/) for the interactive content. (Unlike R Shiny, there is no need to host a R Shiny server!)

To build the site, either use make:

```
cd website
make
```

or, in R:

```
rmarkdown::render_site()
```

The output will be in the `_site` folder. 

The data going into the website, i.e. the coup forecast and map data, are setup and cleaned in [`_data/prepare-data.R`](_data/prepare-data.R). 

Deploying the website: I just manually uploaded the site to GoDaddy using the cAdmin panel. I tried looking into the `ftp` option but couldn't get it to work well. 


