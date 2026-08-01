# Get all relationships types that exist in the OMOP vocabulary `concept_relationship` table for a given set of concepts in a codelist.

Get all relationships types that exist in the OMOP vocabulary
`concept_relationship` table for a given set of concepts in a codelist.

## Usage

``` r
associatedRelationshipIds(
  x,
  cdm,
  standardConcept1 = c("Standard", "Non-standard", "Classification"),
  standardConcept2 = c("Standard", "Non-standard", "Classification"),
  domains1 = NULL,
  domains2 = NULL
)
```

## Arguments

- x:

  A codelist, codelist_with_details, or a concept_set. See
  [`newCodelist()`](https://darwin-eu.github.io/omopgenerics/reference/newCodelist.html),
  [`newCodelistWithDetails()`](https://darwin-eu.github.io/omopgenerics/reference/newCodelistWithDetails.html),
  [`newConceptSetExpression()`](https://darwin-eu.github.io/omopgenerics/reference/newConceptSetExpression.html)
  functions for more details.

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

# Create codelist
codelist <- newCodelist(list("codes1" = c(8479L, 4117795L, 44022939L),
                             "codes2" = c(8480L, 8600L, 8481L, 4189167L, 40371897L)))

# You can optionally restrict to only relationships between concepts that are
# "Standard" and "Non-standard". For example:
relationships <- associatedRelationshipIds(x = codelist,
                                           cdm = cdm,
                                           standardConcept1 = "Standard",
                                           standardConcept2 = "Non-standard")
relationships
#> $codes1
#> [1] "Mapped from"
#> 
#> $codes2
#> [1] "Concept replaces" "Mapped from"     
#> 

# It returns the relationships between concepts where:
#  - concept_id_1 is 'Standard'
#  - concept_id_2 is 'Non-standard'
# Similarly, we can obtain the relationships restricting by domain:
relationships <- associatedRelationshipIds(x = codelist,
                                           cdm = cdm,
                                           domains1 = c("Drug", "Condition"),
                                           domains2 = c("Drug", "Condition"))
relationships
#> $codes1
#> [1] "Brand name of"
#> 
#> $codes2
#> [1] "Concept poss_eq to"
#> 
# }
```
