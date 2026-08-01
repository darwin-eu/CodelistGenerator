# Subset a codelist to only those with a particular dose unit.

Subset a codelist to only those with a particular dose unit.

## Usage

``` r
subsetOnDoseUnit(x, cdm, doseUnit, negate = FALSE)
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

- doseUnit:

  Only codes with the specified dose unit will be returned. If NULL,
  descendant codes will be returned regardless of dose unit Use
  'availableDoseUnits()' to see the available dose units, or
  'associatedDoseUnits()' to see the associated dose forms in a
  codelist.

- negate:

  If FALSE, only concepts with the dose unit specified will be returned.
  If TRUE, concepts with the dose unit specified will be excluded.

## Value

The codelist with only those concepts associated with the dose unit (if
negate = FALSE) or codelist without those concepts associated with the
dose unit(if negate = TRUE).

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omopgenerics)
cdm <- mockVocabRef()
codes <- subsetOnDoseUnit(x = newCodelist(list("codes" = c(20,21))),
                          cdm = cdm,
                          doseUnit = c("milligram"))
#> Warning: ! `codelist` cast to integers.

codes
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - codes (1 codes)
# }
```
