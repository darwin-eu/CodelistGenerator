# Get available drug routes

Get the dose form categories available in the database (see
https://doi.org/10.1002/pds.5809) for more details on how routes were
classified).

## Usage

``` r
availableRouteCategories(cdm)
```

## Arguments

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

## Value

A character vector with all available routes.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create CDM object
cdm <- mockCdmReference()

# Get all domains available in the CDM
availableRouteCategories(cdm = cdm)
#> [1] "topical"            "transmucosal_nasal"
# }
```
