# Generate example vocabulary database

Generate example vocabulary database

## Usage

``` r
mockVocabRef(backend = "data_frame")
```

## Arguments

- backend:

  'database' (duckdb) or 'data_frame'.

## Value

cdm reference with mock vocabulary.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
cdm <- mockVocabRef()
cdm
#> 
#> ── # OMOP CDM reference (local) of mock ────────────────────────────────────────
#> • omop tables: cdm_source, concept, concept_ancestor, concept_relationship,
#> concept_synonym, condition_occurrence, drug_strength, observation_period,
#> person, vocabulary
#> • cohort tables: -
#> • achilles tables: achilles_analysis, achilles_results, achilles_results_dist
#> • other tables: -
# }
```
