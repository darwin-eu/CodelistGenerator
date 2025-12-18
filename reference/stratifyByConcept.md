# Stratify a codelist by the concepts included within it.

Stratify a codelist by the concepts included within it.

## Usage

``` r
stratifyByConcept(
  x,
  cdm,
  nameStyle = "{codelist_name}_{concept}",
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
  codelist name and `{concept}` to include the concept name.

- keepOriginal:

  Whether to keep the original codelist (TRUE) or just return the
  stratified ones (FALSE).

## Value

The codelist or a codelist with details with the required
stratifications, as different elements of the list.

## Examples

``` r
# \donttest{
library(CodelistGenerator)

cdm <- mockVocabRef()

codes <- newCodelist(list("concepts" = c(20L, 21L)))

new_codes <- stratifyByConcept(x = codes,
                               cdm = cdm,
                               keepOriginal = TRUE)

new_codes
#> 
#> ── 3 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - concepts (2 codes)
#> - concepts_glucagon_nasal_powder (1 codes)
#> - concepts_nitrogen_topical_liquefied_gas (1 codes)
# }
```
