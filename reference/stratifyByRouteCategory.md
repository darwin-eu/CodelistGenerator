# Stratify a codelist by route category.

Stratify a codelist by route category.

## Usage

``` r
stratifyByRouteCategory(
  x,
  cdm,
  nameStyle = "{codelist_name}_{route_category}",
  keepOriginal = FALSE
)
```

## Arguments

- x:

  A codelist.

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- nameStyle:

  Naming of the new codelists, use `{codelist_name}` to include the
  codelist name and `{route_category}` to include the route category
  name.

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
library(omopgenerics)
cdm <- mockVocabRef()
codes <- newCodelist(list("concepts" = c(20,21,22)))
#> Warning: ! `codelist` casted to integers.
new_codes <- stratifyByRouteCategory(x = codes,
                                     cdm = cdm,
                                     keepOriginal = TRUE)
new_codes
#> 
#> ── 4 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - concepts (3 codes)
#> - concepts_topical (1 codes)
#> - concepts_transmucosal_nasal (1 codes)
#> - concepts_unclassified_route_category (1 codes)
# }
```
