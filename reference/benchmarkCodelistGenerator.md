# Run benchmark of codelistGenerator analyses

Run benchmark of codelistGenerator analyses

## Usage

``` r
benchmarkCodelistGenerator(cdm)
```

## Arguments

- cdm:

  a CDM reference object

## Value

a tibble with time taken for different analysis

## Examples

``` r
# \donttest{
library(CodelistGenerator)

cdm <- mockVocabRef()

timings <- benchmarkCodelistGenerator(cdm)
#> Warning: - No matching Ingredient codes found for acetaminophen and codein
#> Limiting to domains of interest
#> Getting concepts to include
#> Adding concepts using synonymns
#> Adding codes from non-standard
#> No codes found for the given search strategy
# }
```
