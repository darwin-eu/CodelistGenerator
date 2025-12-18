# Get the concept classes associated with a codelist

Get the concept classes associated with a codelist

## Usage

``` r
associatedConceptClassIds(x, cdm, standardConcept = "Standard", domain = NULL)
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

- domain:

  Character vector with one or more of the OMOP CDM domains. The results
  will be restricted to the given domains. Check the available ones by
  running availableDomains(). If NULL, all supported domains are
  included: Condition, Drug, Procedure, Device, Observation, and
  Measurement.

## Value

The concept classes

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create CDM object
cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#> ℹ Reading GiBleed tables.
#> ℹ Adding drug_strength table.
#> ℹ Creating local <cdm_reference> object.

# Get concept_class_ids in a codelist
x <- newCodelist(list("codes1" = c(1118088L, 40213201L, 35208414L),
                      "codes2" = c(1557272L, 4336464L, 4295880L)))
associatedConceptClassIds(x, cdm,
                         standardConcept = "Standard")
#> $codes1
#> [1] "Branded Drug" "CVX"         
#> 
#> $codes2
#> [1] "Ingredient" "Procedure" 
#> 

# Notice that this corresponds to the information provided by `concept_class_id`
# column in the `concept` table
# }
```
