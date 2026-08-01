# Find orphan codes related to a codelist using achilles counts and, if available, PHOEBE concept recommendations

Find orphan codes related to a codelist using achilles counts and, if
available, PHOEBE concept recommendations

## Usage

``` r
summariseOrphanCodes(
  x,
  cdm,
  domain = c("condition", "device", "drug", "measurement", "observation", "procedure",
    "visit")
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

- domain:

  Character vector with one or more of the OMOP CDM domains. The results
  will be restricted to the given domains. Check the available domains
  in the database by running 'availableDomains()', or
  'associatedDomains()' to explore the domains associated with a
  codelist. If NULL, all supported domains are included: Condition,
  Drug, Procedure, Device, Observation, and Measurement.

## Value

A summarised result containing the frequency of codes related to (but
not in) the codelist.

## Examples

``` r
# \donttest{
library(CodelistGenerator)

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
                          keywords = "Musculoskeletal disorder",
                          domains = "Condition",
                          includeDescendants = FALSE)
#> Limiting to concept type, domains, and vocabularies of interest
#> Getting concepts to include
#> Search completed. Finishing up.
#> ✔ 1 candidate concept identified
#> Time taken: 0 minutes and 0 seconds
codelist <- omopgenerics::newCodelist(list("msk" = codes$concept_id))
orphan_codes <- summariseOrphanCodes(x = codelist,
                                      cdm = cdm)
#> Warning: The domains "Device", "Measurement", "Procedure", and "Visit" are not present
#> in the cdm.
#> Getting orphan codes for msk
#> 

orphan_codes
#> # A tibble: 4 × 13
#>   result_id cdm_name group_name    group_level strata_name strata_level
#>       <int> <chr>    <chr>         <chr>       <chr>       <chr>       
#> 1         1 mock     codelist_name msk         domain_id   condition   
#> 2         1 mock     codelist_name msk         domain_id   condition   
#> 3         1 mock     codelist_name msk         domain_id   condition   
#> 4         1 mock     codelist_name msk         domain_id   condition   
#> # ℹ 7 more variables: variable_name <chr>, variable_level <chr>,
#> #   estimate_name <chr>, estimate_type <chr>, estimate_value <chr>,
#> #   additional_name <chr>, additional_level <chr>
CDMConnector::cdmDisconnect(cdm)
# }
```
