# Get descendant codes for a given concept

Get descendant codes for a given concept

## Usage

``` r
getDescendants(cdm, conceptId, withAncestor = FALSE)
```

## Arguments

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- conceptId:

  concept_id to search

- withAncestor:

  If TRUE, return column with ancestor. In case of multiple ancestors,
  concepts will be separated by ";".

## Value

The descendants of a given concept id.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
cdm <- mockVocabRef()
getDescendants(cdm = cdm, conceptId = 1)
#> # A tibble: 5 × 10
#>   concept_id concept_name             domain_id vocabulary_id standard_concept
#>        <int> <chr>                    <chr>     <chr>         <chr>           
#> 1          1 Musculoskeletal disorder Condition SNOMED        S               
#> 2          2 Osteoarthrosis           Condition SNOMED        S               
#> 3          3 Arthritis                Condition SNOMED        S               
#> 4          4 Osteoarthritis of knee   Condition SNOMED        S               
#> 5          5 Osteoarthritis of hip    Condition SNOMED        S               
#> # ℹ 5 more variables: concept_class_id <chr>, concept_code <chr>,
#> #   valid_start_date <date>, valid_end_date <date>, invalid_reason <chr>
# }
```
