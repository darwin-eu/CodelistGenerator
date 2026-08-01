# Importing and exporting codelists

## Importing and exporting codelists

We will typically want to save the codelists we create for re-use. To
show how we can do this, let’s first create an empty folder to store our
codelists in.

``` r

library(CodelistGenerator)
#> Registered S3 method overwritten by 'CodelistGenerator':
#>   method            from        
#>   print.code_search omopgenerics
```

``` r

dir_codes <- file.path(tempdir(), "codelists")
dir.create(dir_codes)
list.files(dir_codes)
#> character(0)
```

Now let’s create a couple of codelists that we will save.

``` r

codelist <- list("codes1" = c(1L, 2L, 3L),
                 "codes2" = c(4L, 5L, 10L))
codelist <- newCodelist(codelist)

codelist 
#> 
#> ── 2 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - codes1 (3 codes)
#> - codes2 (3 codes)
```

We can use
[`exportCodelist()`](https://darwin-eu.github.io/omopgenerics/reference/exportCodelist.html)
to save these as two CSVs, one for each codelist.

``` r

exportCodelist(codelist, dir_codes, type = "csv")
list.files(dir_codes)
#> [1] "codes1.csv" "codes2.csv"
```

To import codelists, we have
[`importCodelist()`](https://darwin-eu.github.io/omopgenerics/reference/importCodelist.html).
Here we can see that we can easily import our codelists back into R.

``` r

importCodelist(dir_codes, type = "csv")
#> 2 codelists imported.
#> 
#> 
#> ── 2 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - codes1 (3 codes)
#> - codes2 (3 codes)
```

## Importing concept sets

As we’ve seen in the previous vignettes, codelists can also be
represented as concept set which is resolved against the OMOP CDM
vocabulary. To import these we can use
[`importConceptSetExpression()`](https://darwin-eu.github.io/omopgenerics/reference/importConceptSetExpression.html).

Take this example concept set expression.

``` r

library(jsonlite)
concept_set_path <- system.file("concepts_for_mock/arthritis_with_excluded.json", 
                                package = "CodelistGenerator")
fromJSON(concept_set_path) |> toJSON(pretty = TRUE, auto_unbox = TRUE)
#> {
#>   "items": [
#>     {
#>       "concept": {
#>         "CONCEPT_ID": 3
#>       },
#>       "isExcluded": false,
#>       "includeDescendants": true,
#>       "includeMapped": false
#>     },
#>     {
#>       "concept": {
#>         "CONCEPT_ID": 4
#>       },
#>       "isExcluded": true,
#>       "includeDescendants": false,
#>       "includeMapped": false
#>     }
#>   ]
#> }
```

We can bring this into R as a concept set expression.

``` r

cse <- importConceptSetExpression(concept_set_path)
cse
#> 
#> - arthritis_with_excluded (2 concept criteria)
```

And we can then resolve it to a concept set. Note, for this we will need
to specify a cdm reference as the result will be tied to a given OMOP
CDM vocabulary version which will specify the relevant descendants.

``` r

cdm <- mockVocabRef()
importConceptSetExpression(concept_set_path) |> 
  asCodelist(cdm)
#> 
#> - arthritis_with_excluded (2 codes)
```
