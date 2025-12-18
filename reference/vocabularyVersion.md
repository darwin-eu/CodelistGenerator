# Get the available version of the vocabulary used in the cdm

Get the available version of the vocabulary used in the cdm

## Usage

``` r
vocabularyVersion(cdm)
```

## Arguments

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

## Value

The vocabulary version being used in the cdm.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
cdm <- mockVocabRef()
vocabularyVersion(cdm = cdm)
#> [1] "v5.0 22-JUN-22"
# }
```
