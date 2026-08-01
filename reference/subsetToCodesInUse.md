# Filter a codelist to keep only the codes being used in patient records

Filter a codelist to keep only the codes being used in patient records

## Usage

``` r
subsetToCodesInUse(
  x,
  cdm,
  minimumCount = 0L,
  table = c("condition_occurrence", "device_exposure", "drug_exposure", "measurement",
    "observation", "procedure_occurrence", "visit_occurrence")
)
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

- minimumCount:

  Any codes with a frequency under this will be removed.

- table:

  cdm table of interest.

## Value

The filtered codelist with only the codes used in the database

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omopgenerics)
cdm <- mockVocabRef("database")
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpD7xYPw/duckdb
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
codes <- getCandidateCodes(cdm = cdm,
                           keywords = "arthritis",
                           domains = "Condition",
                           includeDescendants = FALSE)
#> Limiting to concept type, domains, and vocabularies of interest
#> Getting concepts to include
#> Search completed. Finishing up.
#> ✔ 3 candidate concepts identified
#> Time taken: 0 minutes and 0 seconds
x <- subsetToCodesInUse(newCodelist(list("cs1" = codes$concept_id,
                               "cs2" = 999)),
                                cdm = cdm)
#> Warning: ! `codelist` cast to integers.
#> No codes from codelist cs2 found in the database
#> Warning: "cs2" codelist will be removed from the final codelist, as there are no
#> elements left after subsetting.

x
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - cs1 (2 codes)
CDMConnector::cdmDisconnect(cdm)
# }
```
