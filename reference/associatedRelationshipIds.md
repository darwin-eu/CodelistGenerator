# Get available relationships with concepts in a codelist

Get available relationships with concepts in a codelist

## Usage

``` r
associatedRelationshipIds(
  x,
  cdm,
  standardConcept1 = "Standard",
  standardConcept2 = "Standard",
  domains1 = "Condition",
  domains2 = "Condition"
)
```

## Arguments

- x:

  A codelist.

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


codelist <- newCodelist(list("codes1" = c(8479L, 4117795L),
                             "codes2" = c(8480L, 8600L, 8481L, 4189167L)))
associatedRelationshipIds(x = codelist, cdm = cdm,
                         standardConcept1 = c("Standard", "Non-standard", "Classification"),
                         standardConcept2 = c("Standard", "Non-standard", "Classification"),
                         domains1 = NULL,
                         domains2 = NULL)
#> $codes1
#> [1] "Is a"        "Mapped from" "Maps to"    
#> 
#> $codes2
#> [1] "Concept replaced by" "Concept replaces"    "Is a"               
#> [4] "Mapped from"         "Maps to"            
#> 
# }
```
