# Report the search strategy used to identify codes when using the `getCandidateCodes()` function

Report the search strategy used to identify codes when using the
[`getCandidateCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/getCandidateCodes.md)
function

## Usage

``` r
searchStrategy(x)
```

## Arguments

- x:

  A codelist.

## Value

A tibble with the search strategy

## Examples

``` r
# \donttest{
library(omock)
library(CodelistGenerator)
library(dplyr, warn.conflicts = FALSE)

# Create CDM object
cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#> ℹ Reading GiBleed tables.
#> ℹ Adding drug_strength table.
#> ℹ Creating local <cdm_reference> object.
codes <- getCandidateCodes(cdm = cdm,
                           keywords = c("sprain", "fracture"),
                           exclude = "knee",
                           domains = "Condition",
                           standardConcept = "Standard",
                           searchNonStandard = FALSE,
                           searchInSynonyms = TRUE,
                           includeDescendants = TRUE,
                           includeAncestor = FALSE)
#> Limiting to domains of interest
#> Getting concepts to include
#> Adding concepts using synonymns
#> Adding descendants
#> Search completed. Finishing up.
#> ✔ 11 candidate concepts identified
#> Time taken: 0 minutes and 0 seconds

searchStrategy(codes) |>
    glimpse()
#> Rows: 1
#> Columns: 10
#> $ cdm_name            <chr> "GiBleed"
#> $ vocabulary_version  <chr> "v5.0 18-JAN-19"
#> $ keywords            <chr> "\"sprain\", \"fracture\""
#> $ exclude             <chr> "\"knee\""
#> $ domains             <chr> "\"condition\""
#> $ standard_concept    <chr> "\"Standard\""
#> $ search_in_synonyms  <lgl> TRUE
#> $ search_non_standard <lgl> FALSE
#> $ include_descendants <lgl> TRUE
#> $ include_ancestor    <lgl> FALSE
# }
```
