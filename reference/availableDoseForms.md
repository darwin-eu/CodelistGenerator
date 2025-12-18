# Get the dose forms for drug concepts

Get the dose forms for drug concepts

## Usage

``` r
availableDoseForms(cdm)
```

## Arguments

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

# Get all domains available in the CDM
availableDoseForms(cdm = cdm)
#> [1] "Nasal Powder"          "Topical Liquefied Gas"
# }
```
