# Subset a codelist to only those codes from a particular vocabulary.

Subset a codelist to only those codes from a particular vocabulary.

## Usage

``` r
subsetOnVocabulary(x, cdm, vocabulary, negate = FALSE)
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

- vocabulary:

  Vocabulary to subset with (i.e., SNOMED)

- negate:

  If FALSE, only concepts with the vocabulary specified will be
  returned. If TRUE, concepts with the vocabulary specified will be
  excluded.

## Value

The codelist with only those concepts associated with the vocabulary (if
negate = FALSE) or the codelist without those concepts associated with
the vocabulary (if negate = TRUE).

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omopgenerics)
cdm <- mockVocabRef()
codes <- subsetOnVocabulary(
              x = newCodelist(list("codes" = c(1L,13L,15L))),
              cdm = cdm,
              vocabulary = "SNOMED")
codes
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - codes (1 codes)
# }
```
