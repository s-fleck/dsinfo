
<!-- README.md is generated from README.Rmd. Please edit that file -->
dsinfo
======

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

A framework to add metadata to any R Object. This package does not provide any exciting functionality, but rather proposes a metadata standard for R objects. The main motivation behind this package was to provide a standardized way to store metadata with R objects that are saved to disk via `saveRDS()` and similar functions.

The metadata format is heavily inspired by [Data Packages](https://frictionlessdata.io/data-packages/). However, **dsinfo** is not designed as a tool to work with actual Data Packages, for that purpose please take a look at [datapackage-r](https://github.com/frictionlessdata/datapackage-r).

Installation
------------

You can install dsinfo from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("s-fleck/dsinfo")
```

Example
-------

``` r

library(dsinfo)

x <- set_dsinfo(
  iris,
  id = "iris001",
  reference_date = Sys.Date(),
  name = "iris_dataset",
  title = "The Iris Dataset",
  version = "1.0",
  sources = dsi_sources(
    dsi_source("R demo data", path = "path/to/data", date = Sys.Date()),
    dsi_source("Alfred Bogus", email = c("alfred@bogus.xx", "alfred.bogus@yahoo.ru"))
  ),
  custom_metadata = "blubb",
  description = paste(rep("blah", 10), collapse = " "),
  homepage = "http://www.blah.bl"
)
  
dsinfo(x)
#> iris001: iris_dataset (1.0 - 2018-04-19) 
#> The Iris Dataset 
#> 
#> blah blah blah blah blah blah blah blah blah blah
#>  
#> sources: 
#>   R demo data (2018-04-19)
#>      - path/to/data
#>   Alfred Bogus
#>      - alfred@bogus.xx
#>      - alfred.bogus@yahoo.ru 
#> homepage:
#>   http://www.blah.bl 
#> custom_metadata:
#>   blubb
```
