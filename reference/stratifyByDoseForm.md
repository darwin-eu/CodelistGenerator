# Stratify a codelist by dose form.

Stratify a codelist by dose form.

## Usage

``` r
stratifyByDoseForm(
  x,
  cdm,
  nameStyle = "{codelist_name}_{dose_form}",
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
  codelist name and `{dose_form}` to include the dose form name.

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

codes <- newCodelist(list("codes" = c(10L, 20L, 21L)))
new_codes <- stratifyByDoseForm(x = codes,
                                cdm = cdm,
                                keepOriginal = TRUE)
new_codes
#> 
#> ── 4 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - codes (3 codes)
#> - codes_injection (1 codes)
#> - codes_nasal_powder (1 codes)
#> - codes_topical_liquefied_gas (1 codes)
# }
```
