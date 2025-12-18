# Summarise code use from achilles counts.

Summarise code use from achilles counts.

## Usage

``` r
summariseAchillesCodeUse(x, cdm, countBy = c("record", "person"))
```

## Arguments

- x:

  A codelist.

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
oa <- getCandidateCodes(cdm = cdm, keywords = "osteoarthritis")
#> Limiting to domains of interest
#> Getting concepts to include
#> Adding descendants
#> Search completed. Finishing up.
#> ✔ 2 candidate concepts identified
#> Time taken: 0 minutes and 0 seconds
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
