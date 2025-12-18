# Subset a codelist to only those codes from a particular domain.

Subset a codelist to only those codes from a particular domain.

## Usage

``` r
stratifyByVocabulary(
  x,
  cdm,
  nameStyle = "{codelist_name}_{vocabulary}",
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
  codelist name and `{vocabulary}` to include the vocabulary name.

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

codes <- stratifyByVocabulary(
  x = newCodelist(list("codes" = c(10L, 13L, 15L))),
  cdm = cdm,
  keepOriginal = TRUE
)

codes
#> 
#> ── 3 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - codes (3 codes)
#> - codes_icd10 (1 codes)
#> - codes_rx_norm (2 codes)
# }
```
