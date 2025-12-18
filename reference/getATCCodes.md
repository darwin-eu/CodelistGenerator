# Get the descendant codes of Anatomical Therapeutic Chemical (ATC) classification codes

Get the descendant codes of Anatomical Therapeutic Chemical (ATC)
classification codes

## Usage

``` r
getATCCodes(
  cdm,
  level = c("ATC 1st"),
  name = NULL,
  nameStyle = "{concept_code}_{concept_name}",
  doseForm = NULL,
  doseUnit = NULL,
  routeCategory = NULL,
  type = "codelist"
)
```

## Arguments

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- level:

  ATC level. Can be one or more of "ATC 1st", "ATC 2nd", "ATC 3rd", "ATC
  4th", and "ATC 5th".

- name:

  ATC name of interest. For example, c("Dermatologicals", "Nervous
  System"), would result in a list of length two with the descendant
  concepts for these two particular ATC groups.

- nameStyle:

  Name style to apply to returned list. Can be one of
  `"{concept_code}"`,`"{concept_id}"`, `"{concept_name}"`, or a
  combination (i.e., `"{concept_code}_{concept_name}"`).

- doseForm:

  Only codes with the specified dose form will be returned. If NULL,
  descendant codes will be returned regardless of dose form. Use
  'doseForms()' to see the available dose forms.

- doseUnit:

  Only codes with the specified dose unit will be returned. If NULL,
  descendant codes will be returned regardless of dose unit Use
  'availableDoseUnits()' to see the available dose units.

- routeCategory:

  Only codes with the specified route will be returned. If NULL,
  descendant codes will be returned regardless of route category. Use
  getRoutes() to find the available route categories.

- type:

  Can be "codelist" or "codelist_with_details".

## Value

Concepts with their format based on the type argument

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create CDM object
cdm <- mockCdmReference()

# Create a codelist with 1st level ATC codes available in the CDM
codelist <- getATCCodes(cdm = cdm,
                        level = "ATC 1st")
codelist
#> 
#> ── 2 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - L_antineoplastic_and_immunomodulating_agents (5 codes)
#> - R_respiratory_system (7 codes)

# Tune the name of the generated codelists
codelist <- getATCCodes(cdm = cdm,
                        level = "ATC 1st",
                        nameStyle = "{concept_name}_{concept_code}")
codelist
#> 
#> ── 2 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - antineoplastic_and_immunomodulating_agents_L (5 codes)
#> - respiratory_system_R (7 codes)

# Search for a specific ATC name of interest
codelist <- getATCCodes(cdm = cdm,
                        level = "ATC 2nd",
                        name = "immunostimulants")
codelist
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - L03_immunostimulants (1 codes)

# Restrict concepts to specific dose forms, dose units, or route categories.
# Remember that you can use `availableDoseForm()`, `availableDoseUnit()` and
# `availableRouteCategory()` to explore your codelist.
codelist <- getATCCodes(cdm = cdm,
                        level = "ATC 2nd",
                        doseForm = NULL,
                        doseUnit = NULL,
                        routeCategory = NULL,)
codelist
#> 
#> ── 10 codelists ────────────────────────────────────────────────────────────────
#> 
#> - L01_antineoplastic_agents (1 codes)
#> - L02_endocrine_therapy (1 codes)
#> - L03_immunostimulants (1 codes)
#> - L04_immunosuppressants (1 codes)
#> - R01_nasal_preparations (1 codes)
#> - R02_throat_preparations (1 codes)
#> along with 4 more codelists

# You can also create directly a codelist_with_details using the argument `type`
codelist <- getATCCodes(cdm = cdm,
                        level = "ATC 1st",
                        type = "codelist_with_details")
codelist
#> 
#> ── 2 codelists with details ────────────────────────────────────────────────────
#> 
#> - L_antineoplastic_and_immunomodulating_agents (5 codes)
#> - R_respiratory_system (7 codes)
# }
```
