# Subset a codelist to only those codes from a particular domain.

Subset a codelist to only those codes from a particular domain.

## Usage

``` r
subsetOnDomain(x, cdm, domain, negate = FALSE)
```

## Arguments

- x:

  A codelist.

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- domain:

  Character vector with one or more of the OMOP CDM domains. The results
  will be restricted to the given domains. Check the available ones by
  running availableDomains(). If NULL, all supported domains are
  included: Condition, Drug, Procedure, Device, Observation, and
  Measurement.

- negate:

  If FALSE, only concepts with the domain specified will be returned. If
  TRUE, concepts with the domain specified will be excluded.

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
codes <- subsetOnDomain(
              x = newCodelist(list("codes" = c(10,13,15))),
              cdm = cdm,
              domain = "Drug")
#> Warning: ! `codelist` casted to integers.
codes
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - codes (2 codes)
# }
```
