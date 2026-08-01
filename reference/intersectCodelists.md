# Generate a codelist from the intersection of different codelists. The generated codelist will come out in alphabetical order.

Generate a codelist from the intersection of different codelists. The
generated codelist will come out in alphabetical order.

## Usage

``` r
intersectCodelists(x, keepOriginal = FALSE)
```

## Arguments

- x:

  A codelist, codelist_with_details, or a concept_set. See
  [`newCodelist()`](https://darwin-eu.github.io/omopgenerics/reference/newCodelist.html),
  [`newCodelistWithDetails()`](https://darwin-eu.github.io/omopgenerics/reference/newCodelistWithDetails.html),
  [`newConceptSetExpression()`](https://darwin-eu.github.io/omopgenerics/reference/newConceptSetExpression.html)
  functions for more details.

- keepOriginal:

  Whether to keep the original codelist (TRUE) or just return the
  stratified ones (FALSE).

## Value

A codelist

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create a CDM object
cdm <- mockCdmReference()

# Intersect two codelists
codelist <- newCodelist(list("mood" = c(37110496L, 4226696L, 4304866L),
                             "manic" = c(37110496L, 4226696L)))

intersectCodelists(codelist, keepOriginal = TRUE)
#> 
#> ── 3 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - intersection_manic_mood (2 codes)
#> - manic (2 codes)
#> - mood (3 codes)


# Intersect two codelists_with_details
codelist <- asCodelistWithDetails(codelist, cdm)

intersectCodelists(codelist, keepOriginal = FALSE)
#> 
#> ── 1 codelist with details ─────────────────────────────────────────────────────
#> 
#> - intersection_manic_mood (2 codes)
# }
```
