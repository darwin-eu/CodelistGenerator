# Get concept ids from JSON files containing concept sets **\[deprecated\]**

Get concept ids from JSON files containing concept sets
**\[deprecated\]**

## Usage

``` r
codesFromConceptSet(path, cdm, type = c("codelist"))
```

## Arguments

- path:

  Path to a file or folder containing JSONs of concept sets.

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- type:

  Can be "codelist", "codelist_with_details" or
  "concept_set_expression".

## Value

Named list with concept_ids for each concept set.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create a CDM object
cdm <- mockCdmReference()

# Load JSON files
x <- codesFromConceptSet(cdm = cdm,
                         path =  system.file(package = "CodelistGenerator",
                                             "concepts_for_mock"))
#> Warning: `codesFromConceptSet()` was deprecated in CodelistGenerator 4.0.0.
#> ℹ Please use omopgenerics::importConceptSetExpression() |> asCodelist()
#>   instead.
x
#> 
#> ── 4 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - arthritis_desc (1 codes)
#> - arthritis_no_desc (1 codes)
#> - arthritis_with_excluded (1 codes)
#> - codelist_with_details (3 codes)

# Load JSON files as codelist_with_details
x <- codesFromConceptSet(cdm = cdm,
                         path =  system.file(package = "CodelistGenerator",
                                             "concepts_for_mock"),
                         type = "codelist_with_details")
#> Warning: Concept_ids "3", "3", "3", "1", "7", and "43054909" are not present in the CDM.
x
#> 
#> ── 4 codelists with details ────────────────────────────────────────────────────
#> 
#> - arthritis_desc (1 codes)
#> - arthritis_no_desc (1 codes)
#> - arthritis_with_excluded (1 codes)
#> - codelist_with_details (3 codes)

# Load JSON files as concept_set_expression
x <- codesFromConceptSet(cdm = cdm,
                         path =  system.file(package = "CodelistGenerator",
                                             "concepts_for_mock"),
                         type = "concept_set_expression")
x
#> 
#> ── 4 concept set expressions ───────────────────────────────────────────────────
#> 
#> - arthritis_desc (1 concept criteria)
#> - arthritis_no_desc (1 concept criteria)
#> - arthritis_with_excluded (2 concept criteria)
#> - codelist_with_details (3 concept criteria)

# }
```
