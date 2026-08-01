# Stratify a codelist by dose unit.

Stratify a codelist by dose unit.

## Usage

``` r
stratifyByDoseUnit(
  x,
  cdm,
  nameStyle = "{codelist_name}_{dose_unit}",
  keepOriginal = FALSE
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

- nameStyle:

  Naming of the new codelists, use `{codelist_name}` to include the
  codelist name and `{dose_unit}` to include the dose unit name.

- keepOriginal:

  Whether to keep the original codelist (TRUE) or just return the
  stratified ones (FALSE).

## Value

The codelist with the required stratifications, as different elements of
the list.

## Examples

``` r
# \donttest{
library(CodelistGenerator)

cdm <- mockVocabRef()

codes <- newCodelist(list("concepts" = c(20L, 21L)))
new_codes <- stratifyByDoseUnit(x = codes,
                                cdm = cdm,
                                keepOriginal = TRUE)
new_codes
#> 
#> ── 3 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - concepts (2 codes)
#> - concepts_milligram (1 codes)
#> - concepts_percent (1 codes)
# }
```
