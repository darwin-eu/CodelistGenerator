# Coerce to a codelist

Coerce to a codelist

## Usage

``` r
asCodelist(x, ...)

# S3 method for class 'codelist'
asCodelist(x, ...)

# S3 method for class 'codelist_with_details'
asCodelist(x, ...)

# S3 method for class 'concept_set_expression'
asCodelist(x, cdm, ...)

# S3 method for class 'candidate_codes'
asCodelist(x, ...)
```

## Arguments

- x:

  Only codelist_with_details and candidate_codes are currently
  supported.

- ...:

  For extensibility

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

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

# Create codelist from a codelist_with_details
codelist <- getDrugIngredientCodes(cdm,
                                   name = "acetaminophen",
                                   nameStyle = "{concept_name}",
                                   type = "codelist_with_details")

asCodelist(codelist)
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen (7 codes)

# Create codelist from a candidate_codes
codelist <- getCandidateCodes(cdm,
                              keywords = "arthritis")
#> Limiting to domains of interest
#> Getting concepts to include
#> Adding descendants
#> Search completed. Finishing up.
#> ✔ 2 candidate concepts identified
#> Time taken: 0 minutes and 0 seconds

asCodelist(codelist)
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - candidate_codes (2 codes)

# }
```
