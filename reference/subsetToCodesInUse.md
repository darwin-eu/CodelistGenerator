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

  A codelist.

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
codes <- getCandidateCodes(cdm = cdm,
                           keywords = "arthritis",
                           domains = "Condition",
                           includeDescendants = FALSE)
#> Limiting to domains of interest
#> Getting concepts to include
#> Search completed. Finishing up.
#> ✔ 3 candidate concepts identified
#> Time taken: 0 minutes and 0 seconds
x <- subsetToCodesInUse(newCodelist(list("cs1" = codes$concept_id,
                               "cs2" = 999)),
                                cdm = cdm)
#> Warning: ! `codelist` casted to integers.
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
