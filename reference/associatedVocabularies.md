# Get the vocabularies associated with a codelist

Get the vocabularies associated with a codelist

## Usage

``` r
associatedVocabularies(x, cdm, standardConcept = "Standard", domain = NULL)
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

- standardConcept:

  Character vector with one or more of "Standard", "Classification", and
  "Non-standard". These correspond to the flags used for the
  standard_concept field in the concept table of the cdm.

- domain:

  Character vector with one or more of the OMOP CDM domains. The results
  will be restricted to the given domains. Check the available domains
  in the database by running 'availableDomains()', or
  'associatedDomains()' to explore the domains associated with a
  codelist. If NULL, all supported domains are included: Condition,
  Drug, Procedure, Device, Observation, and Measurement.

## Value

Names of available vocabularies.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create CDM object
cdm <- mockCdmReference()

# Get all vocabularies from a codelist
codelist <- newCodelist(list("codes1" = c(35604877L, 35604394L),
                             "codes2" = c(4214687L)))
associatedVocabularies(cdm = cdm,
                      x = codelist)
#> $codes1
#> character(0)
#> 
#> $codes2
#> character(0)
#> 
# }
```
