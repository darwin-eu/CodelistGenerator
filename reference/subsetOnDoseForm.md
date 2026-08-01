# Subset a codelist to only those codes from a particular domain.

Subset a codelist to only those codes from a particular domain.

## Usage

``` r
subsetOnDoseForm(x, cdm, doseForm, negate = FALSE)
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

- doseForm:

  Dose form/s. See
  [`availableDoseForms()`](https://darwin-eu.github.io/CodelistGenerator/reference/availableDoseForms.md)
  to explore available dose forms in your codelist.

- negate:

  If FALSE, only concepts with the dose form specified will be returned.
  If TRUE, concepts with the dose form specified will be excluded.

## Value

The codelist with only those concepts associated with the dose form (if
negate = FALSE) or the codelist without those concepts associated with
the dose form (if negate = TRUE).

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omopgenerics)
cdm <- mockVocabRef()

codelist <- newCodelist(list("codes" = c(10L,20L,21L)))

# Dose forms present in our codelist:
codelist |> associatedDoseForms(cdm)
#> $codes
#> [1] "injection"             "nasal_powder"          "topical_liquefied_gas"
#> 

codes <- subsetOnDoseForm(
              x = codelist,
              cdm = cdm,
              doseForm = "Injection")
codes
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - codes (1 codes)

codes |> associatedDoseForms(cdm)
#> $codes
#> [1] "injection"
#> 
# }
```
