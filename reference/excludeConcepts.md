# Exclude concepts from a codelist

Exclude concepts from a codelist

## Usage

``` r
excludeConcepts(x, cdm, concepts, codelistName = NULL)
```

## Arguments

- x:

  A codelist.

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- concepts:

  Concepts_id to exclude

- codelistName:

  Name or names of codelist in x. If NULL, all codelist present in x
  will be considered.

## Value

A codelist

## Examples

``` r
# \donttest{
library(omock)
library(CDMConnector)

# Creating CDM object
# downloadMockDataset(datasetName = "GiBleed")
cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#> ℹ Reading GiBleed tables.
#> ℹ Adding drug_strength table.
#> ℹ Creating local <cdm_reference> object.

# Creating codelist
codelist <- getDrugIngredientCodes(cdm,
                                   nameStyle = "{concept_name}")

# Exclude concept to all the codelists:
codelist$acetaminophen
#> [1]  1125315  1127078  1127433 19133768 40162522 40229134 40231925
codelist <- codelist |>
  excludeConcepts(cdm, concepts = c(1125315L))
codelist$acetaminophen
#> [1]  1127078  1127433 19133768 40162522 40229134 40231925
# Add a concept to a specific codelist
codelist$amiodarone
#> [1] 1309944 1310034
codelist <- codelist |>
  excludeConcepts(cdm, concepts = c(1310034L), codelistName = "amiodarone")
codelist$amiodarone
#> [1] 1309944

# See function: `addConcepts()` for details on how to add specific concepts
# to a codelist
# }
```
