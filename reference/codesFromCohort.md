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
