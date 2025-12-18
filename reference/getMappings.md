# Show mappings from non-standard vocabularies to standard.

Show mappings from non-standard vocabularies to standard.

## Usage

``` r
getMappings(
  candidateCodelist,
  cdm = NULL,
  nonStandardVocabularies = c("ATC", "ICD10CM", "ICD10PCS", "ICD9CM", "ICD9Proc",
    "LOINC", "OPCS4", "Read", "RxNorm", "RxNorm Extension", "SNOMED")
)
```

## Arguments

- candidateCodelist:

  Dataframe.

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- nonStandardVocabularies:

  Character vector.

## Value

Tibble with the information of potential standard to non-standard
mappings for the codelist of interest.

## Examples

``` r
# \donttest{
cdm <- CodelistGenerator::mockVocabRef()
codes <- CodelistGenerator::getCandidateCodes(
  cdm = cdm,
  keywords = "osteoarthritis"
)
#> Limiting to domains of interest
#> Getting concepts to include
#> Adding descendants
#> Search completed. Finishing up.
#> ✔ 2 candidate concepts identified
#> Time taken: 0 minutes and 0 seconds
CodelistGenerator::getMappings(
  cdm = cdm,
  candidateCodelist = codes,
  nonStandardVocabularies = "READ"
)
#> # A tibble: 1 × 7
#>   standard_concept_id standard_concept_name  standard_vocabulary_id
#>                 <int> <chr>                  <chr>                 
#> 1                   4 Osteoarthritis of knee SNOMED                
#> # ℹ 4 more variables: non_standard_concept_id <int>,
#> #   non_standard_concept_name <chr>, non_standard_concept_code <chr>,
#> #   non_standard_vocabulary_id <chr>
# }
```
