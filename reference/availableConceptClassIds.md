# Get the available concept classes used in a given set of domains

Get the available concept classes used in a given set of domains

## Usage

``` r
availableConceptClassIds(cdm, standardConcept = "Standard", domain = NULL)
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

The concept classes

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create CDM object
cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#> ℹ Loading bundled GiBleed tables from package data.
#> ℹ Adding drug_strength table.
#> ℹ Creating local <cdm_reference> object.

# Get all available concept_class_ids in the CDM
availableConceptClassIds(cdm,
                         standardConcept = "Standard")
#>  [1] "Branded Drug"         "Branded Drug Comp"    "Branded Pack"        
#>  [4] "CVX"                  "Clinical Drug"        "Clinical Drug Comp"  
#>  [7] "Clinical Finding"     "Clinical Observation" "Context-dependent"   
#> [10] "Gender"               "Ingredient"           "Lab Test"            
#> [13] "Morph Abnormality"    "Procedure"            "Quant Branded Drug"  
#> [16] "Quant Clinical Drug"  "Visit"               

# Get all available concept_class_ids in the CDM for a specific domain
availableConceptClassIds(cdm,
                         standardConcept = "Standard",
                         domain = "Condition")
#> [1] "Clinical Finding"

# Notice that this corresponds to the information provided by `concept_class_id`
# column in the `concept` table
# }
```
