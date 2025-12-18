# Get available relationships between concepts

Get available relationships between concepts

## Usage

``` r
availableRelationshipIds(
  cdm,
  standardConcept1 = "Standard",
  standardConcept2 = "Standard",
  domains1 = "Condition",
  domains2 = "Condition"
)
```

## Arguments

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- standardConcept1:

  Character vector with one or more of "Standard", "Classification", and
  "Non-standard". These correspond to the flags used for the
  standard_concept field in the concept table of the cdm.

- standardConcept2:

  Character vector with one or more of "Standard", "Classification", and
  "Non-standard". These correspond to the flags used for the
  standard_concept field in the concept table of the cdm.

- domains1:

  Character vector with one or more of the OMOP CDM domain. If NULL, all
  domains are considered.

- domains2:

  Character vector with one or more of the OMOP CDM domain. If NULL, all
  domains are considered.

## Value

A character vector with unique concept relationship values.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create CDM object
cdm <- mockCdmReference()

# Get all relationship ids in the CDM between `Condition` and `Standard` concepts.
availableRelationshipIds(cdm = cdm,
                         standardConcept1 = "Standard",
                         standardConcept2 = "Standard",
                         domains1 = "Condition",
                         domains2 = "Condition")
#> [1] "Is a"        "Mapped from" "Maps to"     "Subsumes"   

# }
```
