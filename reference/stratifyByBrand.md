# Stratify a codelist by brand category.

Stratify a codelist by brand category.

## Usage

``` r
stratifyByBrand(
  x,
  cdm,
  nameStyle = "{codelist_name}_{brand}",
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
  codelist name and `{brand}` to include the brand name.

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
codes <- newCodelist(list(
  concepts_1 = c(20L, 21L, 22L),
  concepts_2 = c(10L, 13L, 21L)
))

new_codes <- stratifyByBrand(x = codes,
                             cdm = cdm,
                             keepOriginal = TRUE)
#> Warning: 1 concept ID have duplicated `brand` and will be assigned to multiple codelists
#> (20).
new_codes
#> 
#> ── 7 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - concepts_1 (3 codes)
#> - concepts_1_brand_1 (1 codes)
#> - concepts_1_brand_2 (2 codes)
#> - concepts_1_unclassified_brand (1 codes)
#> - concepts_2 (3 codes)
#> - concepts_2_brand_2 (2 codes)
#> along with 1 more codelists
# }
```
