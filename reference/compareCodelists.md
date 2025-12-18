# Compare overlap between two sets of codes

Compare overlap between two sets of codes

## Usage

``` r
compareCodelists(codelist1, codelist2)
```

## Arguments

- codelist1:

  Output of getCandidateCodes or a codelist

- codelist2:

  Output of getCandidateCodes.

## Value

Tibble with information on the overlap of codes in both codelists.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omock)

# Create a CDM object
downloadMockDataset(datasetName = "GiBleed",
                    path = NULL,
                    overwrite = NULL)
#> ℹ Deleting prior version of GiBleed.
cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#> ℹ Reading GiBleed tables.
#> ℹ Adding drug_strength table.
#> ℹ Creating local <cdm_reference> object.

# Compare two candidate_codes object
codes1 <- getCandidateCodes(
  cdm = cdm,
  keywords = "Arthritis",
  domains = "Condition",
  includeDescendants = TRUE)
#> Limiting to domains of interest
#> Getting concepts to include
#> Adding descendants
#> Search completed. Finishing up.
#> ✔ 2 candidate concepts identified
#> Time taken: 0 minutes and 0 seconds

codes2 <- getCandidateCodes(
  cdm = cdm,
  keywords = c("osteo"),
  domains = "Condition",
  includeDescendants = TRUE)
#> Limiting to domains of interest
#> Getting concepts to include
#> Adding descendants
#> Search completed. Finishing up.
#> ✔ 3 candidate concepts identified
#> Time taken: 0 minutes and 0 seconds

compareCodelists(
  codelist1 = codes1,
  codelist2 = codes2)
#> # A tibble: 4 × 3
#>   concept_id concept_name                              codelist                 
#>        <int> <chr>                                     <chr>                    
#> 1      80180 Osteoarthritis                            Both                     
#> 2      80502 Osteoporosis                              Only in codelist codelis…
#> 3      80809 Rheumatoid arthritis                      Only in codelist codelis…
#> 4   40480160 Pathological fracture due to osteoporosis Only in codelist codelis…

# Compare two codelists
acetaminophen <- getDrugIngredientCodes(cdm,
                                        name = "acetaminophen",
                                        nameStyle = "{concept_name}",
                                        type = "codelist")

hydrocodone <- getDrugIngredientCodes(cdm,
                                      name = "hydrocodone",
                                      nameStyle = "{concept_name}",
                                      type = "codelist")
compareCodelists(
  codelist1 = acetaminophen,
  codelist2 = hydrocodone)
#> # A tibble: 8 × 3
#>   concept_id concept_name codelist                      
#>        <int> <chr>        <chr>                         
#> 1    1125315 NA           Only in codelist acetaminophen
#> 2    1127078 NA           Only in codelist acetaminophen
#> 3    1127433 NA           Only in codelist acetaminophen
#> 4    1174888 NA           Only in codelist hydrocodone  
#> 5   19133768 NA           Both                          
#> 6   40162522 NA           Both                          
#> 7   40229134 NA           Only in codelist acetaminophen
#> 8   40231925 NA           Only in codelist acetaminophen
# Notice that concept_name = NA as `codelist` class does not store this information
# for each concept.

# Compare two codelists_with_details
acetaminophen <- getDrugIngredientCodes(cdm,
                                        name = "acetaminophen",
                                        nameStyle = "{concept_name}",
                                        type = "codelist_with_details")

hydrocodone <- getDrugIngredientCodes(cdm,
                                      name = "hydrocodone",
                                      nameStyle = "{concept_name}",
                                      type = "codelist_with_details")
compareCodelists(
  codelist1 = acetaminophen,
  codelist2 = hydrocodone)
#> # A tibble: 8 × 3
#>   concept_id concept_name                                               codelist
#>        <int> <chr>                                                      <chr>   
#> 1    1125315 Acetaminophen                                              Only in…
#> 2    1127078 Acetaminophen 160 MG Oral Tablet                           Only in…
#> 3    1127433 Acetaminophen 325 MG Oral Tablet                           Only in…
#> 4    1174888 Hydrocodone                                                Only in…
#> 5   19133768 Acetaminophen 750 MG / Hydrocodone Bitartrate 7.5 MG Oral… Both    
#> 6   40162522 Acetaminophen 325 MG / Hydrocodone Bitartrate 7.5 MG Oral… Both    
#> 7   40229134 Acetaminophen 21.7 MG/ML / Dextromethorphan Hydrobromide … Only in…
#> 8   40231925 Acetaminophen 325 MG / Oxycodone Hydrochloride 5 MG Oral … Only in…

# }
```
