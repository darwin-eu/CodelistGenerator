# Get available dose units

Get the dose units associated with a codelist

## Usage

``` r
associatedDoseUnits(x, cdm, standardConcept = "Standard")
```

## Arguments

- x:

  A codelist.

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- standardConcept:

  Character vector with one or more of "Standard", "Classification", and
  "Non-standard". These correspond to the flags used for the
  standard_concept field in the concept table of the cdm.

## Value

A character vector with available routes.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create CDM object
cdm <- mockCdmReference()

codelist <- newCodelist(list("codes1" = c(194152L, 1830279L, 40558872L),
                             "codes2" = c(44022939L, 1830282L)))
associatedDoseUnits(cdm = cdm,
                   x = codelist)
#> $codes1
#> [1] "milligram"
#> 
#> $codes2
#> [1] "milligram"
#> 
# }
```
