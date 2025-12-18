# Get the dose forms associated with drug concepts in a codelist

Get the dose forms associated with drug concepts in a codelist

## Usage

``` r
associatedDoseForms(x, cdm)
```

## Arguments

- x:

  A codelist.

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

## Value

The dose forms available for drug concepts.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create CDM object
cdm <- mockCdmReference()

# Get all dose forms available in a codelist
codelist <- newCodelist(list("codes1" = c(194152L, 1830279L, 40558872L),
                             "codes2" = c(44022939L)))
associatedDoseForms(x = codelist, cdm = cdm)
#> $codes1
#> [1] "nasal_powder"           "unclassified_dose_form"
#> 
#> $codes2
#> [1] "unclassified_dose_form"
#> 
# }
```
