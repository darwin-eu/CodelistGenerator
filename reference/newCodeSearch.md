# Create a new `code_search` object

**\[experimental\]** This function will be moved to omopgenerics (and
re-exported by CodelistGenerator) once stable.

## Usage

``` r
newCodeSearch(codes, searchStrategy)
```

## Arguments

- codes:

  A tibble with concept ids. It must contain "search_id", "concept_id",
  "found_from", "vocabulary_name", "concept_name", "domain_id",
  "vocabulary_id", "concept_class_id", "standard_concept",
  "concept_code", "valid_start_date", "valid_end_date" and
  "invalid_reason" as columns.

- searchStrategy:

  A tibble with the search strategy used to derive the codes. It must
  contain "search_id", "strategy_name" and "strategy_value" as columns.

## Value

A `code_search` object.
