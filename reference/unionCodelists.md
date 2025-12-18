# Generate a codelist from the union of different codelists. The generated codelist will come out in alphabetical order.

Generate a codelist from the union of different codelists. The generated
codelist will come out in alphabetical order.

## Usage

``` r
unionCodelists(x, keepOriginal = FALSE)
```

## Arguments

- x:

  A codelist.

- keepOriginal:

  Whether to keep the original codelist (TRUE) or just return the
  stratified ones (FALSE).

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
