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

  A codelist.

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- domain:

  Character vector with one or more of the OMOP CDM domains. The results
  will be restricted to the given domains. Check the available ones by
  running availableDomains(). If NULL, all supported domains are
  included: Condition, Drug, Procedure, Device, Observation, and
  Measurement.

## Value

A summarised result containg the frequency of codes related to (but not
in) the codelist.

## Examples

``` r
# \donttest{
library(CodelistGenerator)

cdm <- mockVocabRef("database")
codes <- getCandidateCodes(cdm = cdm,
                          keywords = "Musculoskeletal disorder",
                          domains = "Condition",
                          includeDescendants = FALSE)
#> Limiting to domains of interest
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
