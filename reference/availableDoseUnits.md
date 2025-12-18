# Get available dose units

Get the available dose units

## Usage

``` r
availableDoseUnits(cdm, standardConcept = "Standard")
```

## Arguments

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

# Get all dose units available in the CDM
availableDoseUnits(cdm = cdm)
#> [1] "milligram" "percent"  
# }
```
