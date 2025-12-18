# Get concept ids from JSON files containing cohort definitions

Get concept ids from JSON files containing cohort definitions

## Usage

``` r
codesFromCohort(path, cdm, type = c("codelist"))
```

## Arguments

- path:

  Path to a file or folder containing JSONs of cohort definitions.

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- type:

  Can be "codelist", "codelist_with_details" or
  "concept_set_expression".

## Value

Named list with concept_ids for each concept set.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
cdm <- mockVocabRef("database")
x <- codesFromCohort(cdm = cdm,
                     path =  system.file(package = "CodelistGenerator",
                     "cohorts_for_mock"))
x
#> 
#> ── 3 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - OA no descendants (1 codes)
#> - Other (1 codes)
#> - arthritis (3 codes)
CDMConnector::cdmDisconnect(cdm)
# }
```
