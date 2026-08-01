# Summarise code use from achilles counts.

Summarise code use from achilles counts.

## Usage

``` r
summariseAchillesCodeUse(x, cdm, countBy = c("record", "person"))
```

## Arguments

- x:

  A codelist, codelist_with_details, or a concept_set. See
  [`newCodelist()`](https://darwin-eu.github.io/omopgenerics/reference/newCodelist.html),
  [`newCodelistWithDetails()`](https://darwin-eu.github.io/omopgenerics/reference/newCodelistWithDetails.html),
  [`newConceptSetExpression()`](https://darwin-eu.github.io/omopgenerics/reference/newConceptSetExpression.html)
  functions for more details.

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- countBy:

  Either "record" for record-level counts or "person" for person-level
  counts.

## Value

A tibble with summarised counts.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
cdm <- mockVocabRef("database")
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpApflCg/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> Creating a new cdm
#> Uploading table person (400 rows) - [1/13]
#> Uploading table observation_period (1 rows) - [2/13]
#> Uploading table concept (28 rows) - [3/13]
#> Uploading table condition_occurrence (700 rows) - [4/13]
#> Uploading table concept_ancestor (13 rows) - [5/13]
#> Uploading table concept_synonym (2 rows) - [6/13]
#> Uploading table concept_relationship (17 rows) - [7/13]
#> Uploading table vocabulary (2 rows) - [8/13]
#> Uploading table drug_strength (3 rows) - [9/13]
#> Uploading table cdm_source (1 rows) - [10/13]
#> Uploading table achilles_analysis (21 rows) - [11/13]
#> Uploading table achilles_results (7 rows) - [12/13]
#> Uploading table achilles_results_dist (0 rows) - [13/13]
oa <- getCandidateCodes(cdm = cdm, keywords = "osteoarthritis")
#> Limiting to concept type, domains, and vocabularies of interest
#> Getting concepts to include
#> Adding descendants
#> Search completed. Finishing up.
#> ✔ 2 candidate concepts identified
#> Time taken: 0 minutes and 1 seconds
codelist <- omopgenerics::newCodelist(list(oa = oa$concept_id))
result_achilles <- summariseAchillesCodeUse(codelist, cdm = cdm)
#> 
result_achilles
#> # A tibble: 4 × 13
#>   result_id cdm_name group_name    group_level strata_name strata_level
#>       <int> <chr>    <chr>         <chr>       <chr>       <chr>       
#> 1         1 mock     codelist_name oa          domain_id   condition   
#> 2         1 mock     codelist_name oa          domain_id   condition   
#> 3         1 mock     codelist_name oa          domain_id   condition   
#> 4         1 mock     codelist_name oa          domain_id   condition   
#> # ℹ 7 more variables: variable_name <chr>, variable_level <chr>,
#> #   estimate_name <chr>, estimate_type <chr>, estimate_value <chr>,
#> #   additional_name <chr>, additional_level <chr>
CDMConnector::cdmDisconnect(cdm)
# }
```
