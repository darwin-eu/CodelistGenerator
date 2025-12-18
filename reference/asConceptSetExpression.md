# Coerce to a concept set expression

Coerce to a concept set expression

## Usage

``` r
asConceptSetExpression(x, ...)

# S3 method for class 'codelist'
asConceptSetExpression(x, ...)

# S3 method for class 'codelist_with_details'
asConceptSetExpression(x, ...)
```

## Arguments

- x:

  Codelist or codelist with details

- ...:

  For extensibility

## Value

codelist

## Examples

``` r
# \donttest{
library(omock)
library(CDMConnector)

# Creating CDM object
cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#> ℹ Reading GiBleed tables.
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
