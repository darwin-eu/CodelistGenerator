# Generating vocabulary based codelists for medications

In this vignette, we will explore how to generate codelists for
medications using the OMOP CDM vocabulary tables. To begin, let’s load
the necessary packages and create a cdm reference using Eunomia
synthetic data.

``` r

library(DBI)
library(duckdb)
library(dplyr)
library(CDMConnector)
library(CodelistGenerator)

# Connect to the database and create the cdm object
con <- dbConnect(duckdb(), 
                 eunomiaDir("synpuf-1k", "5.3"))
cdm <- cdmFromCon(con = con, 
                  cdmName = "Eunomia Synpuf",
                  cdmSchema = "main",
                  writeSchema = "main",
                  achillesSchema = "main")
```

## Ingredient based codelists

The
[`getDrugIngredientCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/getDrugIngredientCodes.md)
function can be used to generate the medication codelists based on
ingredient codes.

We can see that we have many drug ingredients for which we could create
codelists.

``` r

availableDrugIngredients(cdm) |> glimpse()
#>  chr [1:15440] ".alpha.-(.alpha.-aminopropyl)benzyl alcohol" ...
```

We will likely be interested in some specific drug ingredients of
interest. Say for example we would like a codelist for acetaminophen
then we can get this easily enough.

``` r

acetaminophen_codes <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("acetaminophen")
)

acetaminophen_codes
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - 161_acetaminophen (22708 codes)
```

Notice that either the concept name or the concept ID can be specified
to find the relevant codes.

``` r

acetaminophen_codes <- getDrugIngredientCodes(
  cdm = cdm,
  name = 1125315
)

acetaminophen_codes
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - 161_acetaminophen (22708 codes)
```

Instead of getting back all concepts for acetaminophen, we can use the
`ingredientRange` argument to return only concepts associated with
acetaminophen and at least one more drug ingredient (i.e. combination
therapies). Here instead of returning a codelist with only the concept
IDs, we will get them with details so that we can see concept names.

``` r

acetaminophen_two_or_more_ingredients <- getDrugIngredientCodes(
  cdm = cdm,
  name = "acetaminophen",
  ingredientRange = c(2, Inf),
  type = "codelist_with_details"
)

acetaminophen_two_or_more_ingredients
#> 
#> ── 1 codelist with details ─────────────────────────────────────────────────────
#> 
#> - 161_acetaminophen (14314 codes)

acetaminophen_two_or_more_ingredients[[1]] |> 
  pull("concept_name") |> 
  head(n = 5) # Only the first five will be shown
#> [1] "Acetaminophen 0.0501 MG/MG / Antipyrine 0.0751 MG/MG / Caffeine 0.0125 MG/MG / Deanol 0.000249 MG/MG Oral Solution"                    
#> [2] "Acetaminophen 0.0501 MG/MG / Antipyrine 0.0751 MG/MG / Caffeine 0.0125 MG/MG / Deanol 0.000249 MG/MG Oral Solution [Sinpro]"           
#> [3] "Acetaminophen 0.0501 MG/MG / Antipyrine 0.0749 MG/MG / Caffeine 0.0125 MG/MG / Deanol 0.000251 MG/MG Oral Solution [Sinpro] Box of 100"
#> [4] "Acetaminophen 0.61 MG/MG / Phenylephrine 0.00742 MG/MG Oral Solution Box of 10"                                                        
#> [5] "Acetaminophen 0.0333 MG/MG / Dextromethorphan 0.001 MG/MG / Phenylephrine 0.000669 MG/MG Oral Solution [Contac]"
```

Or we could instead only return concepts associated with acetaminophen
and no other drug ingredient.

``` r

acetaminophen_one_ingredient <- getDrugIngredientCodes(
  cdm = cdm,
  name = "acetaminophen",
  ingredientRange = c(1, 1),
  type = "codelist_with_details"
)

acetaminophen_one_ingredient
#> 
#> ── 1 codelist with details ─────────────────────────────────────────────────────
#> 
#> - 161_acetaminophen (8394 codes)

acetaminophen_one_ingredient[[1]] |> 
  pull("concept_name") |> 
  head(n = 5) # Only the first five will be shown
#> [1] "50 ML acetaminophen 10 MG/ML Injection"                                    
#> [2] "Acetaminophen Injectable Solution [PARACETAMOL B BRAUN]"                   
#> [3] "Acetaminophen 10 MG/ML Injectable Solution [PARACETAMOL B BRAUN]"          
#> [4] "Acetaminophen 10 MG/ML Injectable Solution [PARACETAMOL B BRAUN] Box of 10"
#> [5] "Acetaminophen 10 MG/ML Injectable Solution [PARACETAMOL B BRAUN] Box of 20"
```

### Restrict to a specific dose form

Perhaps we are just interested in a particular dose form. We can see
that there are many available.

``` r

availableDoseForms(cdm) |> glimpse()
#>  chr [1:162] "Augmented Topical Cream" "Augmented Topical Gel" ...
```

We can choose one or more of these to restrict to a particular dose form
when finding our relevant codes. Here, for example, we only include
codes with a dose form of injection.

``` r

acetaminophen_injections <- getDrugIngredientCodes(
  cdm = cdm,
  name = "acetaminophen",
  doseForm = "injection",
  type = "codelist_with_details"
)

acetaminophen_injections[[1]] |> 
  pull("concept_name") |> 
  head(n = 5) 
#> [1] "50 ML acetaminophen 10 MG/ML Injection"                                               
#> [2] "100 ML acetaminophen 10 MG/ML Injection"                                              
#> [3] "100 ML Acetaminophen 10 MG/ML Injection Box of 10 by A A H"                           
#> [4] "100 ML Acetaminophen 10 MG/ML Injection [Perfalgan] Box of 12 by Bristol Myers Squibb"
#> [5] "100 ML Acetaminophen 10 MG/ML Injection Box of 10"
```

### Restrict to a specific dose unit

Similarly, we can might also want to restrict to a specific dose unit.
Again we have a number of options available in our vocabularies.

``` r

availableDoseUnits(cdm) |> glimpse()
#>  chr [1:29] "50% cell culture infectious dose" ...
```

Here we’ll just include codes with a dose unit of milligram.

``` r

acetaminophen_miligram <- getDrugIngredientCodes(
  cdm = cdm,
  name = "acetaminophen",
  doseUnit = "milligram",
  type = "codelist_with_details"
)

acetaminophen_miligram[[1]] |> 
  pull("concept_name") |> 
  head(n = 5) 
#> [1] "Acetaminophen 0.0501 MG/MG / Antipyrine 0.0751 MG/MG / Caffeine 0.0125 MG/MG / Deanol 0.000249 MG/MG Oral Solution"                    
#> [2] "Acetaminophen 0.0501 MG/MG / Antipyrine 0.0751 MG/MG / Caffeine 0.0125 MG/MG / Deanol 0.000249 MG/MG Oral Solution [Sinpro]"           
#> [3] "Acetaminophen 0.0501 MG/MG / Antipyrine 0.0749 MG/MG / Caffeine 0.0125 MG/MG / Deanol 0.000251 MG/MG Oral Solution [Sinpro] Box of 100"
#> [4] "Acetaminophen 0.61 MG/MG / Phenylephrine 0.00742 MG/MG Oral Solution Box of 10"                                                        
#> [5] "Acetaminophen 0.0333 MG/MG / Dextromethorphan 0.001 MG/MG / Phenylephrine 0.000669 MG/MG Oral Solution [Contac]"
```

### Restrict to a specific route

Lastly, we can restrict to a specific route category. We can see we
again have a number of options.

``` r

availableRouteCategories(cdm) |> glimpse()
#>  chr [1:11] "implant" "inhalable" "injectable" "oral" "topical" ...
```

Here we’ll include only concepts with a route category of inhalable.

``` r

acetaminophen_inhalable <- getDrugIngredientCodes(
  cdm = cdm,
  name = "acetaminophen",
  routeCategory = "inhalable",
  type = "codelist_with_details"
)

acetaminophen_inhalable[[1]] |> 
  pull("concept_name") |> 
  head(n = 5) 
#> [1] "acetaminophen 300 MG Inhalation Powder"
#> [2] "acetaminophen 120 MG Inhalation Powder"
#> [3] "acetaminophen Inhalation Powder"
```

### Search multiple ingredients

The previous examples have focused on single drug ingredient,
acetaminophen. We can though specify multiple ingredients, in which case
we will get a codelist back for each.

``` r

acetaminophen_heparin_codes <- getDrugIngredientCodes(
  cdm = cdm,
  name = c("acetaminophen", "heparin")
  )

acetaminophen_heparin_codes
#> 
#> ── 2 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - 161_acetaminophen (22708 codes)
#> - 5224_heparin (6979 codes)
```

And if we don´t specify an ingredient, we will get a codelist for every
drug ingredient in the vocabularies!

## ATC based codelists

Analogous to
[`getDrugIngredientCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/getDrugIngredientCodes.md),
[`getATCCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/getATCCodes.md)
can be used to generate a codelist based on a particular ATC class.

With ATC we have five levels of the classification which we could be
interested in. The first level is the broadest while the fifth is the
narrowest.

``` r

availableATC(cdm, level = c("ATC 1st")) |> glimpse()
#>  chr [1:14] "ALIMENTARY TRACT AND METABOLISM" ...
availableATC(cdm, level = c("ATC 2nd")) |> glimpse()
#>  chr [1:94] "STOMATOLOGICAL PREPARATIONS" ...
availableATC(cdm, level = c("ATC 3rd")) |> glimpse()
#>  chr [1:267] "STOMATOLOGICAL PREPARATIONS" "ANTACIDS" ...
availableATC(cdm, level = c("ATC 4th")) |> glimpse()
#>  chr [1:890] "Other hematological agents" ...
availableATC(cdm, level = c("ATC 5th")) |> glimpse()
#>  chr [1:5131] "insulin degludec and insulin aspart; parenteral" ...
```

In this example, we will produce an ATC level 1 codelist based on
Alimentary Tract and Metabolism Drugs.

``` r

atc_codelist <- getATCCodes(
  cdm = cdm,
  level = "ATC 1st",
  name = "alimentary tract and metabolism"
)
#> Symbols ",", ".", and "-" will be ignored.
#> `-` will be replaced by an empty space.

atc_codelist
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - A_alimentary_tract_and_metabolism (211265 codes)
```

Similarly as with
[`getDrugIngredientCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/getDrugIngredientCodes.md),
we can use `nameStyle` to specify the name of the elements in the list,
`type` argument to obtain a codelist with details, the `doseForm`
argument to restrict to specific dose forms, the `doseUnit` argument to
restrict to specific dose unit, and the `routeCategory` argument to
restrict to specific route categories.
