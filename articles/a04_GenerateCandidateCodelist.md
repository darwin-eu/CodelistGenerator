# Generate a candidate codelist

In this example we will create a candidate codelist for osteoarthritis,
exploring how different search strategies may impact our final codelist.
First, let’s load the necessary packages and create a cdm reference
using mock data.

``` r

library(dplyr)
library(CodelistGenerator)

cdm <- mockVocabRef()
```

The mock data has the following hypothetical concepts and relationships:

![](Figures/1.png)

## Search for keyword match

We will start by creating a codelist with keywords match. Let’s say that
we want to find those codes that contain “Musculoskeletal disorder” in
their concept_name: ![](Figures/2.png)

``` r

getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal disorder",
  domains = "Condition", 
  standardConcept = "Standard",
  includeDescendants = FALSE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Musculoskeletal disorder",
#>   exclude = NULL,
#>   domains = "condition",
#>   vocabularyId = "snomed",
#>   standardConcept = "Standard",
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = FALSE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 1 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          1 From initi… Musculoskel… v5.0 22-JUN-22     Condition SNOMED       
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>
```

Note that we could also identify it based on a partial match or based on
all combinations match.

``` r

getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal",
  domains = "Condition",
  standardConcept = "Standard",
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeDescendants = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Musculoskeletal",
#>   exclude = NULL,
#>   domains = "condition",
#>   vocabularyId = "snomed",
#>   standardConcept = "Standard",
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = FALSE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 1 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          1 From initi… Musculoskel… v5.0 22-JUN-22     Condition SNOMED       
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>

getCandidateCodes(
  cdm = cdm,
  keywords = "Disorder musculoskeletal",
  domains = "Condition",
  standardConcept = "Standard",
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeDescendants = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Disorder musculoskeletal",
#>   exclude = NULL,
#>   domains = "condition",
#>   vocabularyId = "snomed",
#>   standardConcept = "Standard",
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = FALSE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 1 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          1 From initi… Musculoskel… v5.0 22-JUN-22     Condition SNOMED       
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>
```

Notice that currently we are only looking for concepts with
`domain = "Condition"`. However, we can expand the search to all domains
using `domain = NULL`.

[`getCandidateCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/getCandidateCodes.md)
function will generate a table with class “candidate_codes”, which
contains an attribute with the details of the search strategy:

``` r

candidate_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal",
  domains = "Condition",
  standardConcept = "Standard",
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeDescendants = FALSE,
  includeAncestor = FALSE
)

searchStrategy(candidate_codes)
#> # A tibble: 13 × 3
#>    strategy_id strategy_name      strategy_value       
#>          <int> <chr>              <chr>                
#>  1           1 package_name       "CodelistGenerator"  
#>  2           1 package_version    "4.1.0"              
#>  3           1 function_name      "getCandidateCodes"  
#>  4           1 cdm                "cdm"                
#>  5           1 keywords           "\"Musculoskeletal\""
#>  6           1 exclude            "NULL"               
#>  7           1 domains            "\"condition\""      
#>  8           1 vocabularyId       "\"snomed\""         
#>  9           1 standardConcept    "\"Standard\""       
#> 10           1 searchInSynonyms   "FALSE"              
#> 11           1 searchNonStandard  "FALSE"              
#> 12           1 includeDescendants "FALSE"              
#> 13           1 includeAncestor    "FALSE"
```

## Include non-standard concepts

Now we will include standard and non-standard concepts in our initial
search. By setting `standardConcept = c("Non-standard", "Standard")`, we
allow the function to return, in the final candidate codelist, both the
non-standard and standard codes that have been found.

![](Figures/3.png)

``` r

getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal disorder",
  domains = "Condition",
  standardConcept = c("Non-standard", "Standard"),
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeDescendants = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Musculoskeletal disorder",
#>   exclude = NULL,
#>   domains = "condition",
#>   vocabularyId = c("icd10", "read", "snomed"),
#>   standardConcept = c("Non-standard", "Standard"),
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = FALSE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 2 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          1 From initi… Musculoskel… v5.0 22-JUN-22     Condition SNOMED       
#> 2         24 From initi… Other muscu… v5.0 22-JUN-22     Condition SNOMED       
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>
```

## Multiple search terms

We can also search for multiple keywords simultaneously, capturing all
of them with the following search:

![](Figures/4.png)

``` r

getCandidateCodes(
  cdm = cdm,
  keywords = c(
    "Musculoskeletal disorder",
    "arthritis"
  ),
  domains = "Condition",
  standardConcept = c("Standard"),
  includeDescendants = FALSE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = c("Musculoskeletal disorder", "arthritis"),
#>   exclude = NULL,
#>   domains = "condition",
#>   vocabularyId = "snomed",
#>   standardConcept = "Standard",
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = FALSE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 4 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          1 From initi… Musculoskel… v5.0 22-JUN-22     Condition SNOMED       
#> 2          3 From initi… Arthritis    v5.0 22-JUN-22     Condition SNOMED       
#> 3          4 From initi… Osteoarthri… v5.0 22-JUN-22     Condition SNOMED       
#> 4          5 From initi… Osteoarthri… v5.0 22-JUN-22     Condition SNOMED       
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>
```

## Add descendants

Now we will include the descendants of an identified code using
`includeDescendants` argument ![](Figures/5.png)

``` r

getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal disorder",
  domains = "Condition",
  standardConcept = "Standard",
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Musculoskeletal disorder",
#>   exclude = NULL,
#>   domains = "condition",
#>   vocabularyId = "snomed",
#>   standardConcept = "Standard",
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = TRUE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 5 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          1 From initi… Musculoskel… v5.0 22-JUN-22     Condition SNOMED       
#> 2          2 From desce… Osteoarthro… v5.0 22-JUN-22     Condition SNOMED       
#> 3          3 From desce… Arthritis    v5.0 22-JUN-22     Condition SNOMED       
#> 4          4 From desce… Osteoarthri… v5.0 22-JUN-22     Condition SNOMED       
#> 5          5 From desce… Osteoarthri… v5.0 22-JUN-22     Condition SNOMED       
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>
```

Notice that now, in the column `found_from`, we can see that we have
obtain `concept_id=1` from an initial search, and
`concept_id_=c(2,3,4,5)` when searching for descendants of concept_id 1.

## With exclusions

We can also exclude specific keywords using the argument `exclude`

![](Figures/6.png)

``` r

getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal disorder",
  domains = "Condition",
  exclude = c("Osteoarthrosis", "knee"),
  standardConcept = "Standard",
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Musculoskeletal disorder",
#>   exclude = c("Osteoarthrosis", "knee"),
#>   domains = "condition",
#>   vocabularyId = "snomed",
#>   standardConcept = "Standard",
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = TRUE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 3 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          1 From initi… Musculoskel… v5.0 22-JUN-22     Condition SNOMED       
#> 2          3 From desce… Arthritis    v5.0 22-JUN-22     Condition SNOMED       
#> 3          5 From desce… Osteoarthri… v5.0 22-JUN-22     Condition SNOMED       
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>
```

When multiple words are added within a term (e.g., “knee
osteoarthritis”), each word will be searched independently, so that for
example, “osteoarthritis of knee” is excluded:

``` r

getCandidateCodes(
  cdm = cdm,
  keywords = "Musculoskeletal disorder",
  domains = "Condition",
  exclude = c("knee osteoarthritis"),
  standardConcept = "Standard",
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Musculoskeletal disorder",
#>   exclude = "knee osteoarthritis",
#>   domains = "condition",
#>   vocabularyId = "snomed",
#>   standardConcept = "Standard",
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = TRUE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 4 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          1 From initi… Musculoskel… v5.0 22-JUN-22     Condition SNOMED       
#> 2          2 From desce… Osteoarthro… v5.0 22-JUN-22     Condition SNOMED       
#> 3          3 From desce… Arthritis    v5.0 22-JUN-22     Condition SNOMED       
#> 4          5 From desce… Osteoarthri… v5.0 22-JUN-22     Condition SNOMED       
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>
```

If we only want to exclude exact matching terms (without accounting for
words boundaries) we need to add “/” at the beginning and at the end of
the term. Hence, using “knee osteoarthritis”, “osteoarthritis of knee”
**won’t** be excluded. However, if we had “right knee osteoarthritis”,
it would be excluded.

``` r

# No exclusion:
getCandidateCodes(
    cdm = cdm,
    keywords = "Knee",
    domains = "Condition",
    exclude = NULL,
    standardConcept = c("Standard", "Non-standard"),
    includeDescendants = TRUE,
    searchInSynonyms = FALSE,
    searchNonStandard = FALSE,
    includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Knee",
#>   exclude = NULL,
#>   domains = "condition",
#>   vocabularyId = c("icd10", "read", "snomed"),
#>   standardConcept = c("Standard", "Non-standard"),
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = TRUE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 2 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          4 From initi… Osteoarthri… v5.0 22-JUN-22     Condition SNOMED       
#> 2          8 From initi… Knee osteoa… v5.0 22-JUN-22     Condition Read         
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>

# Exclusion looking for terms:
getCandidateCodes(
  cdm = cdm,
  keywords = "Knee",
  domains = "Condition",
  exclude = c("knee osteoarthritis"),
  standardConcept = c("Standard", "Non-standard"),
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Knee",
#>   exclude = "knee osteoarthritis",
#>   domains = "condition",
#>   vocabularyId = c("icd10", "read", "snomed"),
#>   standardConcept = c("Standard", "Non-standard"),
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = TRUE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 0 × 13
#> # ℹ 13 variables: concept_id <int>, found_from <chr>, concept_name <chr>,
#> #   vocabulary_version <chr>, domain_id <chr>, vocabulary_id <chr>,
#> #   concept_class_id <chr>, standard_concept <chr>, concept_code <chr>,
#> #   valid_start_date <date>, valid_end_date <date>, invalid_reason <chr>,
#> #   found_id <int>

# Exclusion looking for partial matching terms (without word boundaries)
getCandidateCodes(
  cdm = cdm,
  keywords = "Knee",
  domains = "Condition",
  exclude = c("/knee osteoarthritis/"),
  standardConcept = c("Standard", "Non-standard"),
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Knee",
#>   exclude = "/knee osteoarthritis/",
#>   domains = "condition",
#>   vocabularyId = c("icd10", "read", "snomed"),
#>   standardConcept = c("Standard", "Non-standard"),
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = TRUE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 1 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          4 From initi… Osteoarthri… v5.0 22-JUN-22     Condition SNOMED       
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>

# Exclusion looking for partial matching terms (without word boundaries)
getCandidateCodes(
  cdm = cdm,
  keywords = "Knee",
  domains = "Condition",
  exclude = c("/e osteoarthritis/"),
  standardConcept = c("Standard", "Non-standard"),
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Knee",
#>   exclude = "/e osteoarthritis/",
#>   domains = "condition",
#>   vocabularyId = c("icd10", "read", "snomed"),
#>   standardConcept = c("Standard", "Non-standard"),
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = TRUE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 1 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          4 From initi… Osteoarthri… v5.0 22-JUN-22     Condition SNOMED       
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>
```

If we want to do exact matching (that means, to find the exact two words
“knee osteoarthritis” in the concept name) we need to use “/ at the
beginning and at the end of the expression.

``` r

getCandidateCodes(
  cdm = cdm,
  keywords = "Knee",
  domains = "Condition",
  exclude = c("/\bKnee osteoarthritis/\b"),
  standardConcept = c("Standard", "Non-standard"),
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Knee",
#>   exclude = "/Knee osteoarthritis/",
#>   domains = "condition",
#>   vocabularyId = c("icd10", "read", "snomed"),
#>   standardConcept = c("Standard", "Non-standard"),
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = TRUE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 1 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          4 From initi… Osteoarthri… v5.0 22-JUN-22     Condition SNOMED       
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>

# We will now only search for "ee osteoarthritis" to show that 
# "knee osteoarthritis" won't be excluded:
getCandidateCodes(
  cdm = cdm,
  keywords = "Knee",
  domains = "Condition",
  exclude = c("/\bee osteoarthritis/\b"),
  standardConcept = c("Standard", "Non-standard"),
  includeDescendants = TRUE,
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Knee",
#>   exclude = "/ee osteoarthritis/",
#>   domains = "condition",
#>   vocabularyId = c("icd10", "read", "snomed"),
#>   standardConcept = c("Standard", "Non-standard"),
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = TRUE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 2 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          4 From initi… Osteoarthri… v5.0 22-JUN-22     Condition SNOMED       
#> 2          8 From initi… Knee osteoa… v5.0 22-JUN-22     Condition Read         
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>
```


    For example, if we look for 

    Notice that, for example, if we wanted `keywords = "depression"` and `exclude = "ST depression"`, concepts like "poSTpartum depression" would be excluded. To avoid this,
    we could use `exclude = "/ST depression/"`. Notice that, "poST depression" would also be excluded with this option.

    Hence, there is another option to exclude exact matching terms accounting for words boundaries: adding "/\b" at the beginning and at the end of the term. For example, if we look for "/\bp osteoarthritis/\b", concepts like "hip osteoarthritis **won't** be excluded.



    ## Add ancestor
    To include the ancestors one level above the identified concepts, we can use the argument `includeAncestor`
    <img src="Figures/7.png" class="r-plt" alt="" width="100%" />


    ``` r
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Osteoarthritis of knee",
      includeAncestor = TRUE,
      domains = "Condition",
      standardConcept = "Standard",
      includeDescendants = TRUE,
      searchInSynonyms = FALSE,
      searchNonStandard = FALSE,
    )

    codes
    #>  [1;34mi [0m This is a candidate code searh, see: <link to vignette>
    #>  [3mCandidate codes [0m generated using  [32mCodelistGenerator [0m ( [1m4.1.0 [0m):
    #> getCandidateCodes(
    #>   cdm = cdm,
    #>   keywords = "Osteoarthritis of knee",
    #>   exclude = NULL,
    #>   domains = "condition",
    #>   vocabularyId = "snomed",
    #>   standardConcept = "Standard",
    #>   searchInSynonyms = FALSE,
    #>   searchNonStandard = FALSE,
    #>   includeDescendants = TRUE,
    #>   includeAncestor = TRUE
    #> )
    #>  [38;5;246m# A tibble: 2 × 12 [39m
    #>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
    #>  [38;5;250m* [39m       [3m [38;5;246m<int> [39m [23m  [3m [38;5;246m<chr> [39m [23m        [3m [38;5;246m<chr> [39m [23m         [3m [38;5;246m<chr> [39m [23m               [3m [38;5;246m<chr> [39m [23m      [3m [38;5;246m<chr> [39m [23m        
    #>  [38;5;250m1 [39m          3 From ances… Arthritis    v5.0 22-JUN-22     Condition SNOMED       
    #>  [38;5;250m2 [39m          4 From initi… Osteoarthri… v5.0 22-JUN-22     Condition SNOMED       
    #>  [38;5;246m# ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>, [39m
    #>  [38;5;246m#   concept_code <chr>, valid_start_date <date>, valid_end_date <date>, [39m
    #>  [38;5;246m#   invalid_reason <chr> [39m

## Search using synonyms

We can also pick up codes based on their synonyms. For example,
**Osteoarthrosis** has a synonym of **Arthritis**. ![](Figures/8.png)

``` r

getCandidateCodes(
  cdm = cdm,
  keywords = "osteoarthrosis",
  domains = "Condition",
  searchInSynonyms = TRUE,
  standardConcept = "Standard",
  includeDescendants = FALSE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "osteoarthrosis",
#>   exclude = NULL,
#>   domains = "condition",
#>   vocabularyId = "snomed",
#>   standardConcept = "Standard",
#>   searchInSynonyms = TRUE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = FALSE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 2 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          2 From initi… Osteoarthro… v5.0 22-JUN-22     Condition SNOMED       
#> 2          3 In synonyms Arthritis    v5.0 22-JUN-22     Condition SNOMED       
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>
```

Notice that if `includeDescendants = TRUE`, **Arthritis** descendants
will also be included: ![](Figures/9.png)

``` r

getCandidateCodes(
  cdm = cdm,
  keywords = "osteoarthrosis",
  domains = "Condition",
  searchInSynonyms = TRUE,
  standardConcept = "Standard",
  includeDescendants = TRUE,
  searchNonStandard = FALSE,
  includeAncestor = FALSE
)
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "osteoarthrosis",
#>   exclude = NULL,
#>   domains = "condition",
#>   vocabularyId = "snomed",
#>   standardConcept = "Standard",
#>   searchInSynonyms = TRUE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = TRUE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 4 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          2 From initi… Osteoarthro… v5.0 22-JUN-22     Condition SNOMED       
#> 2          3 In synonyms Arthritis    v5.0 22-JUN-22     Condition SNOMED       
#> 3          4 From desce… Osteoarthri… v5.0 22-JUN-22     Condition SNOMED       
#> 4          5 From desce… Osteoarthri… v5.0 22-JUN-22     Condition SNOMED       
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>
```

## Search via non-standard

We can also pick up concepts associated with our keyword via
non-standard search. ![](Figures/10.png)

``` r

codes1 <- getCandidateCodes(
  cdm = cdm,
  keywords = "Degenerative",
  domains = "Condition",
  standardConcept = "Standard",
  searchNonStandard = TRUE,
  includeDescendants = FALSE,
  searchInSynonyms = FALSE,
  includeAncestor = FALSE
)
codes1
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Degenerative",
#>   exclude = NULL,
#>   domains = "condition",
#>   vocabularyId = "snomed",
#>   standardConcept = "Standard",
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = TRUE,
#>   includeDescendants = FALSE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 1 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          2 From non-s… Osteoarthro… v5.0 22-JUN-22     Condition SNOMED       
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>
```

Let’s take a moment to focus on the `standardConcept` and
`searchNonStandard` arguments to clarify the difference between them.
`standardConcept` specifies whether we want only standard concepts or
also include non-standard concepts in the final candidate codelist.
`searchNonStandard` determines whether we want to search for keywords
among non-standard concepts.

In the previous example, since we set `standardConcept = "Standard"`, we
retrieved the code for **Osteoarthrosis** from the non-standard search.
However, we did not obtain the non-standard code **degenerative
arthropathy** from the initial search. If we allow non-standard concepts
in the final candidate codelist, we would retireve both codes:

![](Figures/11.png)

``` r

codes2 <- getCandidateCodes(
  cdm = cdm,
  keywords = "Degenerative",
  domains = "Condition",
  standardConcept = c("Non-standard", "Standard"),
  searchNonStandard = FALSE,
  includeDescendants = FALSE,
  searchInSynonyms = FALSE,
  includeAncestor = FALSE
)
codes2
#> i This is a candidate code searh, see: <link to vignette>
#> Candidate codes generated using CodelistGenerator (4.1.0):
#> getCandidateCodes(
#>   cdm = cdm,
#>   keywords = "Degenerative",
#>   exclude = NULL,
#>   domains = "condition",
#>   vocabularyId = c("icd10", "read", "snomed"),
#>   standardConcept = c("Non-standard", "Standard"),
#>   searchInSynonyms = FALSE,
#>   searchNonStandard = FALSE,
#>   includeDescendants = FALSE,
#>   includeAncestor = FALSE
#> )
#> # A tibble: 1 × 12
#>   concept_id found_from  concept_name vocabulary_version domain_id vocabulary_id
#> *      <int> <chr>       <chr>        <chr>              <chr>     <chr>        
#> 1          7 From initi… Degenerativ… v5.0 22-JUN-22     Condition Read         
#> # ℹ 6 more variables: concept_class_id <chr>, standard_concept <chr>,
#> #   concept_code <chr>, valid_start_date <date>, valid_end_date <date>,
#> #   invalid_reason <chr>
```
