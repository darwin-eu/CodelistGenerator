
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CodelistGenerator

## Introduction

CodelistGenerator is used to create a candidate set of codes for helping
to define patient cohorts in data mapped to the OMOP common data model.
A little like the process for a systematic review, the idea is that for
a specified search strategy, CodelistGenerator will identify a set of
concepts that may be relevant, with these then being screened to remove
any irrelevant codes.

## Installation

You can install the development version of CodelistGenerator like so:

``` r
install.packages("remotes")
remotes::install_github("oxford-pharmacoepi/CodelistGenerator")
```

## Example

Note, Eunomia, used in the example below, does not include a full set of
vocabularies.

``` r
library(CodelistGenerator)
library(dplyr)
library(Eunomia)
library(RSQLite)
library(DBI)
untar(xzfile(system.file("sqlite", "cdm.tar.xz", package = "Eunomia"), open = "rb"),
        exdir =  tempdir())
db <- dbConnect(RSQLite::SQLite(), paste0(tempdir(),"\\cdm.sqlite"))
```

Every codelist is specific to a version of the OMOP CDM vocabularies, so
we can first check the version.

``` r
get_vocab_version(db=db,
                  vocabulary_database_schema = "main")
#> [1] "v5.0 18-JAN-19"
```

We can then search for asthma like so

``` r
get_candidate_codes(keywords="asthma",
                    domains = "Condition",
                    db=db,
                    vocabulary_database_schema = "main")
#> # A tibble: 2 x 4
#>   concept_id concept_name     domain_id vocabulary_id
#>        <dbl> <chr>            <chr>     <chr>        
#> 1    4051466 Childhood asthma Condition SNOMED       
#> 2     317009 Asthma           Condition SNOMED
```

Perhaps we want to exclude asthma in children as part of the search
strategy, in which case this can be added like so

``` r
get_candidate_codes(keywords="asthma",
                    domains = "Condition",
                    exclude = "Childhood asthma",
                    db=db,
                    vocabulary_database_schema = "main")
#> # A tibble: 1 x 4
#>   concept_id concept_name domain_id vocabulary_id
#>        <dbl> <chr>        <chr>     <chr>        
#> 1     317009 Asthma       Condition SNOMED
```
