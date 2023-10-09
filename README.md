
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/CodelistGenerator)](https://CRAN.R-project.org/package=CodelistGenerator)
[![codecov.io](https://codecov.io/github/darwin-eu/CodelistGenerator/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu/CodelistGenerator?branch=main)
[![R-CMD-check](https://github.com/darwin-eu/CodelistGenerator/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/CodelistGenerator/actions)
[![Lifecycle:Stable](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

# CodelistGenerator

## Installation

You can install CodelistGenerator from CRAN

``` r
install.packages("CodelistGenerator")
```

Or you can also install the development version of CodelistGenerator

``` r
install.packages("remotes")
remotes::install_github("darwin-eu/CodelistGenerator")
```

## Example usage

``` r
library(dplyr)
library(CDMConnector)
library(CodelistGenerator)
```

For this example we’ll use the Eunomia dataset (which only contains a
subset of the OMOP CDM vocabularies)

``` r
db <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
cdm <- cdm_from_con(db, cdm_schema = "main", write_schema = c(prefix = "cg_", schema = "main"))
```

## Exploring the OMOP CDM Vocabulary tables

OMOP CDM vocabularies are frequently updated, and we can identify the
version of the vocabulary of our Eunomia data

``` r
getVocabVersion(cdm = cdm)
#> [1] "v5.0 18-JAN-19"
```

CodelistGenerator provides various other functions to explore the
vocabulary tables. For example, we can see the the different concept
classes of standard concepts used for drugs

``` r
getConceptClassId(cdm,
                  standardConcept = "Standard",
                  domain = "Drug")
#> [1] "Branded Drug"        "CVX"                 "Ingredient"         
#> [4] "Clinical Drug"       "Branded Pack"        "Quant Branded Drug" 
#> [7] "Quant Clinical Drug" "Branded Drug Comp"   "Clinical Drug Comp"
```

## Vocabulary based codelists using CodelistGenerator

CodelistGenerator provides functions to extract code lists based on
vocabulary hierarchies. One example is \`getDrugIngredientCodes, which
we can use, for example, to get all the concept IDs used to represent
aspirin.

``` r
getDrugIngredientCodes(cdm = cdm, name = "aspirin")
#> $`Ingredient: Aspirin (1112807)`
#> [1]  1112807 19059056
```

If we also want the details of these concept IDs we can get these like
so.

``` r
getDrugIngredientCodes(cdm = cdm, name = "aspirin", withConceptDetails = TRUE)
#> $`Ingredient: Aspirin (1112807)`
#> # A tibble: 2 × 4
#>   concept_id concept_name              domain_id vocabulary_id
#>        <dbl> <chr>                     <chr>     <chr>        
#> 1    1112807 Aspirin                   Drug      RxNorm       
#> 2   19059056 Aspirin 81 MG Oral Tablet Drug      RxNorm
```

And if we want codelists for all drug ingredients we can simply omit the
name argument and all ingredients will be returned.

``` r
ing <- getDrugIngredientCodes(cdm = cdm)
ing$aspirin
#> NULL
ing$diclofenac
#> NULL
ing$celecoxib
#> NULL
```

## Systematic search using CodelistGenerator

CodelistGenerator can also support systematic searches of the vocabulary
tables to support codelist development. A little like the process for a
systematic review, the idea is that for a specified search strategy,
CodelistGenerator will identify a set of concepts that may be relevant,
with these then being screened to remove any irrelevant codes by
clinical experts.

We can do a simple search for asthma

``` r
asthma_codes1 <- getCandidateCodes(
  cdm = cdm,
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

But perhaps we want to exclude certain concepts as part of the search
strategy, in this case we can add these like so

``` r
asthma_codes2 <- getCandidateCodes(
  cdm = cdm,
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
standard snowmed code for gastrointestinal hemorrhage returned by our
search

``` r
Gastrointestinal_hemorrhage <- getCandidateCodes(
  cdm = cdm,
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

## Summarising code use

``` r
summariseCodeUse(asthma_codes1$concept_id,  
                 cdm = cdm) %>% 
  glimpse()
#> Rows: 230
#> Columns: 10
#> $ group_name          <chr> "Codelist", "By concept", "By concept", "Codelist"…
#> $ group_level         <chr> "Overall", "Childhood asthma (4051466)", "Asthma (…
#> $ strata_name         <chr> "Overall", "Overall", "Overall", "Year", "Year", "…
#> $ strata_level        <chr> "Overall", "Overall", "Overall", "1914", "1915", "…
#> $ variable_name       <chr> "Record count", "Record count", "Record count", "R…
#> $ variable_level      <chr> "Overall", "Overall", "Overall", "Overall", "Overa…
#> $ variable_type       <chr> "Numeric", "Numeric", "Numeric", "Numeric", "Numer…
#> $ estimate_type       <chr> "Count", "Count", "Count", "Count", "Count", "Coun…
#> $ estimate            <int> 101, 96, 5, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ estimate_suppressed <chr> "FALSE", "FALSE", "FALSE", "TRUE", "TRUE", "TRUE",…
```
