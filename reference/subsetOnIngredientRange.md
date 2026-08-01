# Subset a codelist to only those codes with a range of number of ingredients

Subset a codelist to only those codes with a range of number of
ingredients

## Usage

``` r
subsetOnIngredientRange(x, cdm, ingredientRange, negate = FALSE)
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

- ingredientRange:

  Used to restrict descendant codes to those associated with a specific
  number of drug ingredients. Must be a vector of length two with the
  first element the minimum number of ingredients allowed and the second
  the maximum. A value of c(2, 2) would restrict to only concepts
  associated with two ingredients.

- negate:

  If FALSE, only concepts with the ingredient range specified will be
  returned (both limits included). If TRUE, concepts with number of
  ingredients outside the range will be returned.

## Value

The codelist with only those concepts associated with the domain (if
negate = FALSE) or the codelist without those concepts associated with
the domain (if negate = TRUE).

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omopgenerics)
cdm <- mockVocabRef()
codes <- subsetOnIngredientRange(
              x = newCodelist(list("codes" = c(10L, 13L))),
              cdm = cdm,
              ingredientRange = c(2, 10))
codes
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - codes (1 codes)
# }
```
