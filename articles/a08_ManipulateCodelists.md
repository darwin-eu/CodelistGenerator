# Manipulate codelists

\## Introduction: Manipulate codelists This vignette introduces a set of
functions designed to manipulate and explore codelists within an OMOP
CDM. Specifically, we will learn how to:

- **Subset a codelist** to keep only codes meeting a certain criteria.
- **Stratify a codelist** based on attributes like dose unit or route of
  administration.
- **Add or exclude** concepts from a codelists.
- **Compare two codelists** to identify shared and unique concepts.

First of all, we will load the required packages and connect to a mock
database.

``` r
library(DBI)
library(duckdb)
library(dplyr)
library(CDMConnector)
library(CodelistGenerator)

# Download mock database
requireEunomia(datasetName = "synpuf-1k", cdmVersion = "5.3")

# Connect to the database and create the cdm object
con <- dbConnect(duckdb(), eunomiaDir("synpuf-1k", "5.3"))
cdm <- cdmFromCon(con = con, 
cdmName = "Eunomia Synpuf",
cdmSchema   = "main",
writeSchema = "main", 
achillesSchema = "main")
```

We will start by generating a codelist for *acetaminophen* using
[`getDrugIngredientCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/getDrugIngredientCodes.md)

``` r
acetaminophen <- getDrugIngredientCodes(cdm,
                                        name = "acetaminophen",
                                        nameStyle = "{concept_name}",
                                        type = "codelist")
```

### Subsetting a Codelist

Subsetting a codelist will allow us to reduce a codelist to only those
concepts that meet certain conditions.

#### Subset by Domain

We will now subset to those concepts that have `domain = "Drug"`.
Remember that, to see the domains available in your codelist, you can
use
[`associatedDomains()`](https://darwin-eu.github.io/CodelistGenerator/reference/associatedDomains.md).

``` r
acetaminophen_drug <- subsetOnDomain(acetaminophen, 
                                     cdm, 
                                     domain = "Drug")

acetaminophen_drug
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen (22708 codes)
```

We can use the `negate` argument to exclude concepts with a certain
domain:

``` r
acetaminophen_no_drug <- subsetOnDomain(acetaminophen, 
                                        cdm, 
                                        domain = "Drug", 
                                        negate = TRUE)

acetaminophen_no_drug
#> 
#> ── 0 codelists ─────────────────────────────────────────────────────────────────
```

#### Subset on vocabulary

We will now subset the codelist to only include concepts from RxNorm
vocabulary. You can also use
[`associatedVocabularies()`](https://darwin-eu.github.io/CodelistGenerator/reference/associatedVocabularies.md)
to explore the vocabularies available in your codelist.

``` r
acetaminophen_rxnorm <- subsetOnVocabulary(acetaminophen_drug, 
                                           cdm, 
                                           c("RxNorm"))
acetaminophen_rxnorm
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen (3025 codes)
```

#### Subset on Dose Unit

We will now filter to only include concepts with specified dose units.
Remember that you can use
[`associatedDoseUnits()`](https://darwin-eu.github.io/CodelistGenerator/reference/associatedDoseUnits.md)
to explore the dose units available in your codelist.

``` r
acetaminophen_mg_unit <- subsetOnDoseUnit(acetaminophen_rxnorm, 
                                          cdm, 
                                          c("milligram", "unit"))
acetaminophen_mg_unit
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen (2918 codes)
```

As before, we can use argument `negate = TRUE` to exclude instead.

#### Subset on ingredient range

We can now subset on those drugs with 3 to 30 ingredients:

``` r
acetaminophen_ingredient <- subsetOnIngredientRange(acetaminophen_drug, 
                                                cdm,
                                                ingredientRange = c(3, 30))

acetaminophen_ingredient
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen (7116 codes)
```

Notice that `negate = TRUE` would keep all those concepts with less than
3 ingredients or more than 30 (without including those with 3 or 30
ingredients).

#### Subset on route category

We will now subset to those concepts that do not have an
“unclassified_route” or “transmucosal_rectal”. See
[`associatedRouteCategories()`](https://darwin-eu.github.io/CodelistGenerator/reference/associatedRouteCategories.md)
to explore route categories available in your codelist.

``` r
acetaminophen_route <- subsetOnRouteCategory(acetaminophen_mg_unit, 
                                             cdm, 
                                             c("transmucosal_rectal","unclassified_route"), 
                                             negate = TRUE)
acetaminophen_route
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen (2850 codes)
```

#### Subset on dose forms

We will now subset to those concepts with specific dose forms. See
`associatedDseForms()` to explore dose forms available in your codelist.

``` r
# First, check which dose forms are available in our codelist
acetaminophen_drug |> 
  associatedDoseForms(cdm)
#> $acetaminophen
#>  [1] "chewable_tablet"              "delayed_release_oral_capsule"
#>  [3] "delayed_release_oral_tablet"  "disintegrating_oral_tablet"  
#>  [5] "effervescent_oral_tablet"     "enema"                       
#>  [7] "extended_release_oral_tablet" "granules_for_oral_solution"  
#>  [9] "inhalation_powder"            "injectable_solution"         
#> [11] "injection"                    "intravenous_solution"        
#> [13] "oral_capsule"                 "oral_granules"               
#> [15] "oral_powder"                  "oral_solution"               
#> [17] "oral_suspension"              "oral_tablet"                 
#> [19] "oral_wafer"                   "powder_for_oral_solution"    
#> [21] "powder_for_oral_suspension"   "prefilled_syringe"           
#> [23] "rectal_suppository"           "tablet_for_oral_suspension"  
#> [25] "topical_gel"                  "topical_solution"            
#> [27] "unclassified_dose_form"
acetaminophen_oral <- subsetOnDoseForm(acetaminophen_drug, 
                                            cdm, 
                                            c("Oral Solution","Oral Capsule"))
acetaminophen_oral 
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen (4008 codes)
```

### Stratify codelist

Instead of filtering, stratification allows us to split a codelist into
subgroups based on defined vocabulary properties.

#### Stratify by Dose Unit

``` r
acetaminophen_doses <- stratifyByDoseUnit(acetaminophen, cdm, keepOriginal = TRUE)

acetaminophen_doses
#> 
#> ── 4 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen (22708 codes)
#> - acetaminophen_milligram (22256 codes)
#> - acetaminophen_unclassified_dose_unit (452 codes)
#> - acetaminophen_unit (1 codes)
```

#### Stratify by Route Category

``` r
acetaminophen_routes <- stratifyByRouteCategory(acetaminophen, cdm)

acetaminophen_routes
#> 
#> ── 6 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen_inhalable (3 codes)
#> - acetaminophen_injectable (689 codes)
#> - acetaminophen_oral (17219 codes)
#> - acetaminophen_topical (6 codes)
#> - acetaminophen_transmucosal_rectal (1459 codes)
#> - acetaminophen_unclassified_route_category (3332 codes)
```

#### Stratify by Dose Form

``` r
acetaminophen_dose_forms <- stratifyByDoseForm(acetaminophen, cdm)

acetaminophen_dose_forms
#> 
#> ── 27 codelists ────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen_chewable_tablet (90 codes)
#> - acetaminophen_delayed_release_oral_capsule (95 codes)
#> - acetaminophen_delayed_release_oral_tablet (528 codes)
#> - acetaminophen_disintegrating_oral_tablet (517 codes)
#> - acetaminophen_effervescent_oral_tablet (531 codes)
#> - acetaminophen_enema (4 codes)
#> along with 21 more codelists
```

## Add or remove concepts from a codelist

We can also add specific concepts to our codelist. For example, we will
add the ingredient “acetaminophen” to all our codelists:

``` r
acetaminophen_routes1 <- addConcepts(acetaminophen_routes, 
                                     cdm,
                                     concepts = c(1125315L))
acetaminophen_routes1
#> 
#> ── 6 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen_inhalable (4 codes)
#> - acetaminophen_injectable (690 codes)
#> - acetaminophen_oral (17220 codes)
#> - acetaminophen_topical (7 codes)
#> - acetaminophen_transmucosal_rectal (1460 codes)
#> - acetaminophen_unclassified_route_category (3332 codes)
```

Or we can add acetaminophen + descendants, and only to some of the
codelists

``` r
x <- getDescendants(cdm = cdm, conceptId = c(1125315L))
acetaminophen_routes2 <- addConcepts(acetaminophen_routes, 
                                     cdm,
                                     concepts = x$concept_id, 
                                     codelistName = "acetaminophen_unclassified_route_category")
acetaminophen_routes2
#> 
#> ── 6 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen_inhalable (3 codes)
#> - acetaminophen_injectable (689 codes)
#> - acetaminophen_oral (17219 codes)
#> - acetaminophen_topical (6 codes)
#> - acetaminophen_transmucosal_rectal (1459 codes)
#> - acetaminophen_unclassified_route_category (23935 codes)
```

And similarly, we can exclude specific concepts and their descendants
from our codelist:

``` r
acetaminophen_routes3 <- excludeConcepts(acetaminophen_routes, 
                                         cdm,
                                         concepts = x$concept_id, 
                                         codelistName = "acetaminophen_inhalable")
acetaminophen_routes3
#> 
#> ── 5 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen_injectable (689 codes)
#> - acetaminophen_oral (17219 codes)
#> - acetaminophen_topical (6 codes)
#> - acetaminophen_transmucosal_rectal (1459 codes)
#> - acetaminophen_unclassified_route_category (3332 codes)
```

Notice that in this case, the codelist “acetaminophen_inhalable” is
removed as there are no elements left after the exclusion of the code
35873016 and its descendants.

### Codelist construction

Notice that all the functions introduced previously are “pipeble”,
allowing for a tidy and clear codelist construction:

``` r
acetaminophen <- getDrugIngredientCodes(cdm,
                                        name = "acetaminophen",
                                        nameStyle = "{concept_name}",
                                        type = "codelist")

new_codelist <- acetaminophen |>
  addConcepts(cdm, 
              concepts = c(1L, 2L, 3L)) |>
  subsetOnDomain(cdm,
                 domain = "Drug") |>
  stratifyByDoseUnit(cdm = cdm) |>
  excludeConcepts(cdm,
                  concepts = c(1127898)) 

new_codelist
#> 
#> ── 2 codelists ─────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen_milligram (22255 codes)
#> - acetaminophen_unclassified_dose_unit (452 codes)
```

### Compare codelists

Now we will compare two codelists to identify overlapping and unique
codes.

``` r
acetaminophen <- getDrugIngredientCodes(cdm, 
                                        name = "acetaminophen", 
                                        nameStyle = "{concept_name}",
                                        type = "codelist_with_details")
hydrocodone <- getDrugIngredientCodes(cdm, 
                                      name = "hydrocodone", 
                                      doseUnit = "milligram", 
                                      nameStyle = "{concept_name}",
                                      type = "codelist_with_details")
```

Compare the two sets:

``` r
comparison <- compareCodelists(acetaminophen,
                               hydrocodone)

comparison |> glimpse()
#> Rows: 24,242
#> Columns: 3
#> $ concept_id   <int> 587290, 587473, 587705, 587929, 588401, 588590, 588717, 5…
#> $ concept_name <chr> "Acetaminophen 0.0501 MG/MG / Antipyrine 0.0751 MG/MG / C…
#> $ codelist     <chr> "Only in codelist acetaminophen", "Only in codelist aceta…

comparison |> filter(codelist == "Both")
#> # A tibble: 253 × 3
#>    concept_id concept_name                                              codelist
#>         <int> <chr>                                                     <chr>   
#>  1    1129026 acetaminophen 500 MG / hydrocodone bitartrate 5 MG Oral … Both    
#>  2    2071492 acetaminophen 500 MG / hydrocodone 7.5 MG Oral Tablet [H… Both    
#>  3    2071493 acetaminophen 500 MG / hydrocodone 7.5 MG Oral Tablet [H… Both    
#>  4    2071494 acetaminophen 500 MG / hydrocodone 7.5 MG Oral Tablet [Z… Both    
#>  5    2071495 acetaminophen 500 MG / hydrocodone 7.5 MG Oral Tablet [Z… Both    
#>  6    2071499 acetaminophen 500 MG / hydrocodone 5 MG Oral Tablet [HYC… Both    
#>  7    2071500 acetaminophen 500 MG / hydrocodone 5 MG Oral Tablet [HYC… Both    
#>  8    2071501 acetaminophen 500 MG / hydrocodone 5 MG [HYCODONE]        Both    
#>  9    2071502 acetaminophen / hydrocodone Oral Tablet [ZYDON]           Both    
#> 10   19133768 acetaminophen 750 MG / hydrocodone bitartrate 7.5 MG Ora… Both    
#> # ℹ 243 more rows
```
