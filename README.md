
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/CodelistGenerator)](https://CRAN.R-project.org/package=CodelistGenerator)
[![codecov.io](https://codecov.io/github/darwin-eu/CodelistGenerator/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu/CodelistGenerator?branch=main)
[![R-CMD-check](https://github.com/darwin-eu/CodelistGenerator/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/CodelistGenerator/actions)
[![Lifecycle:Stable](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

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
remotes::install_github("darwin-eu/CodelistGenerator")
```

## Example usage

``` r
library(dplyr)
library(CDMConnector)
library(CodelistGenerator)
library(kableExtra)
```

In this example we’ll use the Eunomia dataset (which only contains a
subset of the OMOP CDM vocabularies)

``` r
db <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
cdm <- cdm_from_con(db, cdm_schema = "main")
```

Although we can run the search using vocabulary tables in the database
or loaded into R, the fastest approach is using arrow. So let’s create a
new cdm reference using arrow (in this example saved to the temp
directory, but in practice you could of course save files elsewhere for
reuse).

``` r
# save cdm vocabulary tables to temp directory
dOut<-here::here(tempdir(), "db_vocab")
dir.create(dOut)
CDMConnector::stow(cdm, dOut)
# new cdm reference using arrow
cdm_arrow <- CDMConnector::cdm_from_files(path = dOut, 
                                          as_data_frame = FALSE)
```

Every code list is specific to a version of the OMOP CDM vocabularies,
so we can first check the version for Eunomia.

``` r
getVocabVersion(cdm = cdm_arrow)
#> [1] "v5.0 18-JAN-19"
```

We can then search for asthma like so

``` r
asthma_codes1 <- getCandidateCodes(
  cdm = cdm_arrow,
  keywords = "asthma",
  domains = "Condition"
) 
asthma_codes1 %>% 
  glimpse()
#> Rows: 2
#> Columns: 6
#> $ concept_id       <dbl> 4051466, 317009
#> $ concept_name     <chr> "Childhood asthma", "Asthma"
#> $ domain_id        <chr> "condition", "condition"
#> $ concept_class_id <chr> "clinical finding", "clinical finding"
#> $ vocabulary_id    <chr> "snomed", "snomed"
#> $ found_from       <chr> "From initial search", "From initial search"
```

Perhaps we want to exclude certain concepts as part of the search
strategy, in which case this can be added like so

``` r
asthma_codes2 <- getCandidateCodes(
  cdm = cdm_arrow,
  keywords = "asthma",
  exclude = "childhood",
  domains = "Condition"
) 
asthma_codes2 %>% 
  glimpse()
#> Rows: 1
#> Columns: 6
#> $ concept_id       <dbl> 317009
#> $ concept_name     <chr> "Asthma"
#> $ domain_id        <chr> "condition"
#> $ concept_class_id <chr> "clinical finding"
#> $ vocabulary_id    <chr> "snomed"
#> $ found_from       <chr> "From initial search"
```

We can compare these two code lists like so

``` r
compareCodelists(asthma_codes1, asthma_codes2)
#> # A tibble: 2 × 3
#>   concept_id concept_name     codelist       
#>        <dbl> <chr>            <chr>          
#> 1    4051466 Childhood asthma Only codelist 1
#> 2     317009 Asthma           Both
```

We can then also see non-standard codes these are mapped from, for
example here we can see the non-standard ICD10 code that maps to a
standard snomed code for gastrointestinal hemorrhage returned by our
search

``` r
Gastrointestinal_hemorrhage <- getCandidateCodes(
  cdm = cdm_arrow,
  keywords = "Gastrointestinal hemorrhage",
  domains = "Condition"
)
Gastrointestinal_hemorrhage %>% 
  glimpse()
#> Rows: 1
#> Columns: 6
#> $ concept_id       <dbl> 192671
#> $ concept_name     <chr> "Gastrointestinal hemorrhage"
#> $ domain_id        <chr> "condition"
#> $ concept_class_id <chr> "clinical finding"
#> $ vocabulary_id    <chr> "snomed"
#> $ found_from       <chr> "From initial search"
```

``` r
getMappings(
  cdm = cdm_arrow,
  candidateCodelist = Gastrointestinal_hemorrhage,
  nonStandardVocabularies = "ICD10CM"
) %>% 
  glimpse()
#> Rows: 1
#> Columns: 7
#> $ standard_concept_id        <dbl> 192671
#> $ standard_concept_name      <chr> "Gastrointestinal hemorrhage"
#> $ standard_vocabulary_id     <chr> "SNOMED"
#> $ non_standard_concept_id    <dbl> 35208414
#> $ non_standard_concept_name  <chr> "Gastrointestinal hemorrhage, unspecified"
#> $ non_standard_concept_code  <chr> "K92.2"
#> $ non_standard_vocabulary_id <chr> "ICD10CM"
```
