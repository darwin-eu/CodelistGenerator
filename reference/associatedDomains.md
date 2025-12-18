# Get the domains associated with a codelist

Get the domains associated with a codelist

## Usage

``` r
associatedDomains(x, cdm, standardConcept = "Standard")
```

## Arguments

- x:

  A codelist.

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- standardConcept:

  Character vector with one or more of "Standard", "Classification", and
  "Non-standard". These correspond to the flags used for the
  standard_concept field in the concept table of the cdm.

## Value

A vector with the domains of the cdm.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create CDM object
cdm <- mockCdmReference()

# Get all domains available in a codelist
codelist <- newCodelist(list("codes1" = c(194152L, 1830279L, 40558872L),
                             "codes2" = c(44022939L)))
associatedDomains(x = codelist, cdm = cdm,
                 standardConcept = c("Non-standard", "Standard"))
#> $codes1
#> [1] "Condition" "Drug"      "Procedure"
#> 
#> $codes2
#> [1] "Drug"
#> 
# }
```
