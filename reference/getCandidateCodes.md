# Perform a systematic search to identify a candidate codelist using the OMOP CDM vocabulary tables.

Based on the given search strategy, this function identifies a set of
codes that may represent a clinical event of interest in data mapped to
the OMOP CDM. These codes can then be considered for creating a study
phenotype.

## Usage

``` r
getCandidateCodes(
  cdm,
  keywords,
  exclude = NULL,
  domains = "Condition",
  standardConcept = "Standard",
  searchInSynonyms = FALSE,
  searchNonStandard = FALSE,
  includeDescendants = TRUE,
  includeAncestor = FALSE
)
```

## Arguments

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- keywords:

  Character vector of terms to search for.

  - The search performed is broad, matching the provided keywords as
    substrings anywhere within concept names. For example,
    `keywords = c("sep")` will find "sepsis", "aseptic necrosis", and
    "acquired cardiac septal defect" (along with many more).

  - Where more than one word is given, all combinations of those words
    will be identified. For example,
    `keywords = c("knee osteoarthritis")` will identify a concept with
    the name "osteoarthritis of knee".

  - Multiple keywords can be provided. For example,
    `keywords = c("knee osteoarthritis", "hip osteoarthritis")` would
    identify both "osteoarthritis of knee" and "osteoarthritis of hip"

- exclude:

  Character vector of words to identify concepts to exclude. For
  example,
  `getCandidateCodes(cdm, keywords = "septic", exclude = "aseptic", domains = "condition")`
  would remove concepts "aseptic" when seaching for concepts with
  "septic" in their name.

  - When one term contains multiple words (e.g., "knee osteoarthritis"),
    each word will be search individually, so that "osteoarthritis of
    knee" would also be excluded. If you only want to exclude partial
    matching terms, please add "/" at the beginning and the end of each
    term (e.g., `"/knee osteoarthritis/`"). Notice that, with this
    options, concepts like "rightknee osteoarthritis" will also be
    excluded (as this is a partial match), but "osteoarthritis of knee"
    won't be excluded. Different terms can have different rules (e.g.,
    c("hip osteoarthritis", "/knee osteoarthritis/")).

  - With multiple words, if we want exact matches accounting for word
    boundaries, we need to use `/\b` at the beginning and at the end of
    each expression. In the previous example, using
    `"/bknee osteoarthritis/\b"`, "rightknee osteoarthritis" won't be
    excluded, but "History of knee osteoarthritis" will be excluded.

- domains:

  Character vector with one or more of the OMOP CDM domain for which to
  search within. If NULL, all domains are included in the search. Use
  `availableDomains(cdm = cdm)` to identify available domains to search
  within.

- standardConcept:

  Character vector with one or more of "Standard", "Classification", and
  "Non-standard". These correspond to the flags used for the
  standard_concept field in the concept table of the cdm.

- searchInSynonyms:

  Either TRUE or FALSE. If TRUE the code will also search using both the
  primary name in the concept table and synonyms from the concept
  synonym table.

- searchNonStandard:

  Either TRUE or FALSE. If TRUE the code will also search via
  non-standard concepts.

- includeDescendants:

  Either TRUE or FALSE. If TRUE descendant concepts of identified
  concepts will be included in the candidate codelist. If FALSE only
  direct mappings from ICD-10 codes to standard codes will be returned.

- includeAncestor:

  Either TRUE or FALSE. If TRUE the direct ancestor concepts of
  identified concepts will be included in the candidate codelist.

## Value

A "candidate_codes" object. This includes a tibble with the potential
codes of interest, along with an attribute containing the search
strategy.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
cdm <- mockVocabRef()
getCandidateCodes(
  cdm = cdm,
  keywords = "osteoarthritis"
 )
#> Limiting to domains of interest
#> Getting concepts to include
#> Adding descendants
#> Search completed. Finishing up.
#> ✔ 2 candidate concepts identified
#> Time taken: 0 minutes and 0 seconds
#> # A tibble: 2 × 6
#>   concept_id found_from    concept_name domain_id vocabulary_id standard_concept
#>        <int> <chr>         <chr>        <chr>     <chr>         <chr>           
#> 1          4 From initial… Osteoarthri… Condition SNOMED        S               
#> 2          5 From initial… Osteoarthri… Condition SNOMED        S               
# }
```
