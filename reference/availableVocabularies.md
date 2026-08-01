# Get the available vocabularies available in the cdm

Get the available vocabularies available in the cdm

## Usage

``` r
availableVocabularies(cdm, standardConcept = "Standard", domain = NULL)
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

# Get all vocabularies available in the CDM
availableVocabularies(cdm)
#> [1] "Ethnicity"        "Gender"           "LOINC"            "Race"            
#> [5] "RxNorm"           "RxNorm Extension" "SNOMED"           "UCUM"            
#> [9] "Visit"           

# Get all vocabularies available in the CDM for `Standard` and `Condition` concepts
availableVocabularies(cdm,
                      standardConcept = "Standard",
                      domain = "Condition")
#> [1] "SNOMED"
# }
```
