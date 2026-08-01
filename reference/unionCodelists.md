# Generate a codelist from the union of different codelists. The generated codelist will come out in alphabetical order.

Generate a codelist from the union of different codelists. The generated
codelist will come out in alphabetical order.

## Usage

``` r
unionCodelists(
  x,
  newCodelistName = NULL,
  keepOriginal = FALSE,
  codelistsToJoin = names(x)
)
```

## Arguments

- x:

  A codelist, codelist_with_details, or a concept_set. See
  [`newCodelist()`](https://darwin-eu.github.io/omopgenerics/reference/newCodelist.html),
  [`newCodelistWithDetails()`](https://darwin-eu.github.io/omopgenerics/reference/newCodelistWithDetails.html),
  [`newConceptSetExpression()`](https://darwin-eu.github.io/omopgenerics/reference/newConceptSetExpression.html)
  functions for more details.

- newCodelistName:

  Character vector with the name of the new codelist. If NULL all
  codelists names will be combined.

- keepOriginal:

  Whether to keep the original codelist (TRUE) or just return the
  stratified ones (FALSE).

- codelistsToJoin:

  Character vector with the names of the codelists to be unioned.

## Value

A codelist

## Examples

``` r
# \donttest{
library(CodelistGenerator)
cdm <- mockVocabRef()
getDrugIngredientCodes(cdm,
                        nameStyle = "{concept_name}") |>
unionCodelists()
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - adalimumab_other_ingredient (2 codes)
# }
```
