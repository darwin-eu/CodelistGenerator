# Subset a codelist to only those with a particular route category

Subset a codelist to only those with a particular route category

## Usage

``` r
subsetOnRouteCategory(x, cdm, routeCategory, negate = FALSE)
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

- routeCategory:

  Only codes with the specified route will be returned. If NULL,
  descendant codes will be returned regardless of route category. Use
  'availableRouteCategories()' to find the available route categories in
  the database, and 'associatedRouteCategories()' to get drug routs
  associated with a codelist.

- negate:

  If FALSE, only concepts with the routeCategory specified will be
  returned. If TRUE, concepts with the routeCategory specified will be
  excluded.

## Value

The codelist with only those concepts associated with the specified
route categories (if negate is FALSE) or the codelist without those
concepts associated with the specified route categories (if negate is
TRUE).

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omopgenerics)
cdm <- mockVocabRef()
codes <- subsetOnRouteCategory(
              x = newCodelist(list("codes" = c(20,21))),
              cdm = cdm,
              routeCategory = "topical")
#> Warning: ! `codelist` cast to integers.
codes
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - codes (1 codes)
# }
```
