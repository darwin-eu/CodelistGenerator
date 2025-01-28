
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CodelistGenerator <img src="man/figures/logo.png" align="right" height="200"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/CodelistGenerator)](https://CRAN.R-project.org/package=CodelistGenerator)
[![codecov.io](https://codecov.io/github/darwin-eu/CodelistGenerator/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu/CodelistGenerator?branch=main)
[![R-CMD-check](https://github.com/darwin-eu/CodelistGenerator/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/CodelistGenerator/actions)
[![Lifecycle:stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

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
db <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
cdm <- cdmFromCon(db, 
                  cdmSchema = "main", 
                  writeSchema = "main", 
                  writePrefix = "cg_")
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
#> [1] "Branded Drug"        "Branded Drug Comp"   "Branded Pack"       
#> [4] "Clinical Drug"       "Clinical Drug Comp"  "CVX"                
#> [7] "Ingredient"          "Quant Branded Drug"  "Quant Clinical Drug"
```

## Vocabulary based codelists using CodelistGenerator

CodelistGenerator provides functions to extract code lists based on
vocabulary hierarchies. One example is \`getDrugIngredientCodes, which
we can use, for example, to get all the concept IDs used to represent
aspirin.

``` r
getDrugIngredientCodes(cdm = cdm, name = "aspirin", nameStyle = "{concept_name}")
#> 
#> - aspirin (2 codes)
```

And if we want codelists for all drug ingredients we can simply omit the
name argument and all ingredients will be returned.

``` r
ing <- getDrugIngredientCodes(cdm = cdm, nameStyle = "{concept_name}")
ing$aspirin
#> [1] 19059056  1112807
ing$diclofenac
#> [1] 1124300
ing$celecoxib
#> [1] 1118084
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
asthma_codes1 |> 
  glimpse()
#> Rows: 2
#> Columns: 6
#> $ concept_id       <int> 4051466, 317009
#> $ found_from       <chr> "From initial search", "From initial search"
#> $ concept_name     <chr> "Childhood asthma", "Asthma"
#> $ domain_id        <chr> "Condition", "Condition"
#> $ vocabulary_id    <chr> "SNOMED", "SNOMED"
#> $ standard_concept <chr> "S", "S"
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
asthma_codes2 |> 
  glimpse()
#> Rows: 1
#> Columns: 6
#> $ concept_id       <int> 317009
#> $ found_from       <chr> "From initial search"
#> $ concept_name     <chr> "Asthma"
#> $ domain_id        <chr> "Condition"
#> $ vocabulary_id    <chr> "SNOMED"
#> $ standard_concept <chr> "S"
```

We can compare these two code lists like so

``` r
compareCodelists(asthma_codes1, asthma_codes2)
#> # A tibble: 2 × 3
#>   concept_id concept_name     codelist       
#>        <int> <chr>            <chr>          
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
Gastrointestinal_hemorrhage |> 
  glimpse()
#> Rows: 1
#> Columns: 6
#> $ concept_id       <int> 192671
#> $ found_from       <chr> "From initial search"
#> $ concept_name     <chr> "Gastrointestinal hemorrhage"
#> $ domain_id        <chr> "Condition"
#> $ vocabulary_id    <chr> "SNOMED"
#> $ standard_concept <chr> "S"
```

## Summarising code use

``` r
summariseCodeUse(list("asthma" = asthma_codes1$concept_id),  
                 cdm = cdm) |> 
  glimpse()
#> Rows: 6
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1, 1, 1
#> $ cdm_name         <chr> "An OMOP CDM database", "An OMOP CDM database", "An O…
#> $ group_name       <chr> "codelist_name", "codelist_name", "codelist_name", "c…
#> $ group_level      <chr> "asthma", "asthma", "asthma", "asthma", "asthma", "as…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "overall", "Asthma", "Childhood asthma", "overall", "…
#> $ variable_level   <chr> NA, "317009", "4051466", NA, "4051466", "317009"
#> $ estimate_name    <chr> "record_count", "record_count", "record_count", "pers…
#> $ estimate_type    <chr> "integer", "integer", "integer", "integer", "integer"…
#> $ estimate_value   <chr> "101", "5", "96", "101", "96", "5"
#> $ additional_name  <chr> "source_concept_name &&& source_concept_id &&& source…
#> $ additional_level <chr> "NA &&& NA &&& NA &&& NA", "Asthma &&& 317009 &&& 195…
```
