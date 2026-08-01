# Coerce to a concept set expression

Coerce to a concept set expression

## Usage

``` r
asConceptSetExpression(x)

# S3 method for class 'codelist'
asConceptSetExpression(x)

# S3 method for class 'codelist_with_details'
asConceptSetExpression(x)
```

## Arguments

- x:

  A codelist, codelist_with_details, concept_set_expression, or a
  candidate_codes.

## Value

codelist

## Examples

``` r
# \donttest{
library(omock)
library(CDMConnector)

# Creating CDM object
cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#> ℹ Loading bundled GiBleed tables from package data.
#> ℹ Adding drug_strength table.
#> ℹ Creating local <cdm_reference> object.

# Create concept_set_expression from a codelist
codelist <- getDrugIngredientCodes(cdm,
                                   name = "acetaminophen",
                                   nameStyle = "{concept_name}",
                                   type = "codelist")

asConceptSetExpression(codelist)
#> 
#> ── 1 concept set expression ────────────────────────────────────────────────────
#> 
#> - acetaminophen (7 concept criteria)

# Create concept_set_expression from a codelist_with_details
codelist <- getDrugIngredientCodes(cdm,
                                   name = "acetaminophen",
                                  nameStyle = "{concept_name}",
                                   type = "codelist_with_details")

asConceptSetExpression(codelist)
#> 
#> ── 1 concept set expression ────────────────────────────────────────────────────
#> 
#> - acetaminophen (7 concept criteria)
# }
```
