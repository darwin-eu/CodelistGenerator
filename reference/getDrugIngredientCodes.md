# Get descendant codes of drug ingredients

Get descendant codes of drug ingredients

## Usage

``` r
getDrugIngredientCodes(
  cdm,
  name = NULL,
  nameStyle = "{concept_code}_{concept_name}",
  doseForm = NULL,
  doseUnit = NULL,
  routeCategory = NULL,
  ingredientRange = c(1, Inf),
  type = "codelist"
)
```

## Arguments

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- name:

  Names of ingredients of interest. For example, c("acetaminophen",
  "codeine"), would result in a list of length two with the descendant
  concepts for these two particular drug ingredients. Users can also
  specify the concept ID instead of the name (e.g., c(1125315,
  42948451)) using a numeric vector.

- nameStyle:

  Name style to apply to returned list. Can be one of
  `"{concept_code}"`,`"{concept_id}"`, `"{concept_name}"`, or a
  combination (i.e., `"{concept_code}_{concept_name}"`).

- doseForm:

  Only codes with the specified dose form will be returned. If NULL,
  descendant codes will be returned regardless of dose form. Use
  'availableDoseForms()' to see the available dose forms in the
  database, or 'associatedDoseForms()' to see the associated dose forms
  in a codelist.

- doseUnit:

  Only codes with the specified dose unit will be returned. If NULL,
  descendant codes will be returned regardless of dose unit Use
  'availableDoseUnits()' to see the available dose units, or
  'associatedDoseUnits()' to see the associated dose forms in a
  codelist.

- routeCategory:

  Only codes with the specified route will be returned. If NULL,
  descendant codes will be returned regardless of route category. Use
  'availableRouteCategories()' to find the available route categories in
  the database, and 'associatedRouteCategories()' to get drug routs
  associated with a codelist.

- ingredientRange:

  Used to restrict descendant codes to those associated with a specific
  number of drug ingredients. Must be a vector of length two with the
  first element the minimum number of ingredients allowed and the second
  the maximum. A value of c(2, 2) would restrict to only concepts
  associated with two ingredients.

- type:

  Can be "codelist" or "codelist_with_details".

## Value

Concepts with their format based on the type argument.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
cdm <- mockVocabRef()
getDrugIngredientCodes(cdm = cdm,
                       name = "Adalimumab",
                       nameStyle = "{concept_name}")
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - adalimumab (2 codes)
# }
```
