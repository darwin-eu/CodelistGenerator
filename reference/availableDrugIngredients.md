# Get the names of all available drug ingredients

Get the names of all available drug ingredients

## Usage

``` r
availableDrugIngredients(cdm, standardConcept = "Standard")
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

A vector containing the concept names for all ingredient level codes
found in the concept table of cdm.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create CDM object
cdm <- mockCdmReference()

# Get all drug ingredients available in the CDM for standard concepts
availableDrugIngredients(cdm = cdm)
#> [1] "Bos taurus catalase preparation" "potassium sodium tartrate"      
# }
```
