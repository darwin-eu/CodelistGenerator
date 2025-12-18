# Get the names of drug ingredients associated with codelist

Get the names of drug ingredients associated with codelist

## Usage

``` r
associatedDrugIngredients(x, cdm, standardConcept = "Standard")
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

A vector containing the concept names for all ingredient level codes
found in the concept table of cdm.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create CDM object
cdm <- mockCdmReference()

# Get all drug ingredients associated with a codelist
codelist <- newCodelist(list("codes1" = c(37498042L),
                             "codes2" = c( 42899580L, 35741956L)))
associatedDrugIngredients(x = codelist, cdm = cdm,
                         standardConcept = c("Standard", "Non-standard"))
#> $codes1
#> [1] "Bos taurus catalase preparation"
#> 
#> $codes2
#> [1] "potassium sodium tartrate"
#> 
# }
```
