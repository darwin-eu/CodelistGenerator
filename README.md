
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CodelistGenerator

## Introduction

The goal of CodelistGenerator is to create a candidate set of codes for
generating a phenotype for the OMOP CDM. As well as using the package in
R, a corresponding RShiny app for creating candidate codelists using the
OMOP CDM condition
(<https://dpa-pde-oxford.shinyapps.io/OmopCodelistGeneratorConditions/>),
observation (to add), measurement (to add), and procedures (to add) are
available.

## Installation

You can install the development version of CodelistGenerator like so:

``` r
install.packages("remotes")
remotes::install_github("oxford-pharmacoepi/CodelistGenerator")
```

## Example

# Note, Eunomia, used in the example below, does not include a full set of vocabularies.

``` r
library(CodelistGenerator)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.1.2
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(Eunomia)
#> Loading required package: DatabaseConnector
library(stringr)
library(readr)
#> Warning: package 'readr' was built under R version 4.1.2
library(DBI)
devtools::load_all()
#> i Loading CodelistGenerator
#> Warning: package 'testthat' was built under R version 4.1.2
untar(xzfile(system.file("sqlite", "cdm.tar.xz", package = "Eunomia"), open = "rb"),
        exdir =  tempdir())
db <- dbConnect(RSQLite::SQLite(), paste0(tempdir(),"\\cdm.sqlite"))
get_candidate_codes(keywords="asthma",
                    db=db,
                    vocabulary_database_schema = "main")
#> [1] "Limiting to potential concepts of interest (database side)"
#> [1] "Bringing filtered tables into memory"
#> [1] "Getting concepts to include from exact matches"
#> [1] "Getting concepts to include from descendants of identified concepts"
#> [1] "Getting candidate codelist took 0 minutes and 0 seconds"
#> # A tibble: 3 x 4
#>   concept_id concept_name     domain_id vocabulary_id
#>        <dbl> <chr>            <chr>     <chr>        
#> 1    4051466 Childhood asthma Condition SNOMED       
#> 2     317009 Asthma           Condition SNOMED       
#> 3    4062501 Asthma screening Procedure SNOMED
```
