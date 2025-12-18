# Coerce to a codelist with details

Coerce to a codelist with details

## Usage

``` r
asCodelistWithDetails(x, cdm, ...)

# S3 method for class 'codelist_with_details'
asCodelistWithDetails(x, ...)

# S3 method for class 'codelist'
asCodelistWithDetails(x, cdm, ...)

# S3 method for class 'candidate_codes'
asCodelistWithDetails(x, cdm, ...)
```

## Arguments

- x:

  Only codelist and candidate_codes are currently supported.

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

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
path <- downloadMockDataset(datasetName = "GiBleed",
                            path = NULL,
                            overwrite = NULL)
#> ℹ Deleting prior version of GiBleed.
cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#> ℹ Reading GiBleed tables.
#> ℹ Adding drug_strength table.
#> ℹ Creating local <cdm_reference> object.

# Create codelist_with_details from a codelist
codelist <- getDrugIngredientCodes(cdm,
                                   name = "acetaminophen",
                                   nameStyle = "{concept_name}",
                                   type = "codelist")

asCodelistWithDetails(codelist, cdm)
#> 
#> ── 1 codelist with details ─────────────────────────────────────────────────────
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

asCodelistWithDetails(codelist)
#> 
#> ── 1 codelist with details ─────────────────────────────────────────────────────
#> 
#> - candidate_codes (2 codes)
# }
```
