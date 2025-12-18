# Table showing the route category associated with each dose form.

Table showing the route category associated with each dose form.

## Usage

``` r
doseFormToRoute
```

## Format

A data frame

- dose_form_concept_id:

  Concept ID of each dose form

- dose_form_concept_name:

  Concept name of each dose form

- route_category:

  Route category associated to the dose form

## Examples

``` r
# \donttest{
library(CodelistGenerator)
doseFormToRoute
#> # A tibble: 194 × 3
#>    dose_form_concept_id dose_form_concept_name           route_category
#>                   <dbl> <chr>                            <chr>         
#>  1             19103220 12 hour Extended Release Capsule oral          
#>  2             19082048 12 hour Extended Release Tablet  oral          
#>  3             19082049 16 Hour Transdermal Patch        transdermal   
#>  4             19082256 24 Hour Extended Release Capsule oral          
#>  5             19082050 24 Hour Extended Release Tablet  oral          
#>  6             19082071 24 Hour Transdermal Patch        transdermal   
#>  7             19082072 72 Hour Transdermal Patch        transdermal   
#>  8             19135438 Augmented Topical Cream          topical       
#>  9             19135446 Augmented Topical Gel            topical       
#> 10             19135439 Augmented Topical Lotion         topical       
#> # ℹ 184 more rows
# }
```
