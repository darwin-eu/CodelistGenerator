# Get the domains available in the cdm

Get the domains available in the cdm

## Usage

``` r
availableDomains(cdm, standardConcept = "Standard")
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

A vector with the domains of the cdm.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create CDM object
cdm <- mockCdmReference()

# Get all domains available in the CDM for standard concepts
availableDomains(cdm = cdm, standardConcept = "Standard")
#>  [1] "Condition"   "Drug"        "Ethnicity"   "Gender"      "Meas Value" 
#>  [6] "Measurement" "Observation" "Procedure"   "Race"        "Unit"       
#> [11] "Visit"      
# }
```
