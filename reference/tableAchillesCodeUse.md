# Format the result of summariseAchillesCodeUse into a table

Format the result of summariseAchillesCodeUse into a table

## Usage

``` r
tableAchillesCodeUse(
  result,
  type = NULL,
  header = c("cdm_name", "estimate_name"),
  groupColumn = character(),
  hide = character(),
  style = NULL,
  .options = list()
)
```

## Arguments

- result:

  A `<summarised_result>` with results of the type "achilles_code_use".

- type:

  Type of desired formatted table. By default (type = NULL), it will
  create a 'gt' table. To see supported formats use
  visOmopResults::tableType().

- header:

  A vector specifying the elements to include in the header. The order
  of elements matters, with the first being the topmost header. The
  header vector can contain one of the following variables: "cdm_name",
  "codelist_name", "domain_id", "standard_concept_name",
  "standard_concept_id", "estimate_name", "standard_concept",
  "vocabulary_id". Alternatively, it can include other names to use as
  overall header labels.

- groupColumn:

  Variables to use as group labels. Allowed columns are: "cdm_name",
  "codelist_name", "domain_id", "standard_concept_name",
  "standard_concept_id", "estimate_name", "standard_concept",
  "vocabulary_id". These cannot be used in header.

- hide:

  Table columns to exclude, options are: "cdm_name", "codelist_name",
  "domain_id", "standard_concept_name", "standard_concept_id",
  "estimate_name", "standard_concept", "vocabulary_id". These cannot be
  used in header or groupColumn.

- style:

  A character string or custom R code to define the visual formatting of
  the table. This argument can be provided in two ways: (1) Pre-defined
  Styles (Character String): Use a name for a built-in style (e.g.,
  "darwin"). See visOmopResults::tableStyle() for available options. (2)
  Custom Code (Advanced): Supply a block of custom R code. This code
  must be specific to the table type. See
  visOmopResults::tableStyleCode() for structural examples.

- .options:

  Named list with additional formatting options.
  visOmopResults::tableOptions() shows allowed arguments and their
  default values.

## Value

A table with a formatted version of the summariseCohortCodeUse result.

## Examples

``` r
# \donttest{
library(CodelistGenerator)
library(omopgenerics)

cdm <- mockVocabRef("database")
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpD7xYPw/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> Creating a new cdm
#> Uploading table person (400 rows) - [1/13]
#> Uploading table observation_period (1 rows) - [2/13]
#> Uploading table concept (28 rows) - [3/13]
#> Uploading table condition_occurrence (700 rows) - [4/13]
#> Uploading table concept_ancestor (13 rows) - [5/13]
#> Uploading table concept_synonym (2 rows) - [6/13]
#> Uploading table concept_relationship (17 rows) - [7/13]
#> Uploading table vocabulary (2 rows) - [8/13]
#> Uploading table drug_strength (3 rows) - [9/13]
#> Uploading table cdm_source (1 rows) - [10/13]
#> Uploading table achilles_analysis (21 rows) - [11/13]
#> Uploading table achilles_results (7 rows) - [12/13]
#> Uploading table achilles_results_dist (0 rows) - [13/13]
oa <- getCandidateCodes(cdm = cdm, keywords = "osteoarthritis")
#> Limiting to concept type, domains, and vocabularies of interest
#> Getting concepts to include
#> Adding descendants
#> Search completed. Finishing up.
#> ✔ 2 candidate concepts identified
#> Time taken: 0 minutes and 0 seconds
result_achilles <- summariseAchillesCodeUse(newCodelist(list(oa = oa$concept_id)),
                                            cdm = cdm)
#> 
tableAchillesCodeUse(result_achilles)


  

```
