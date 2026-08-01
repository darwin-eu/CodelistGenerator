# Format the result of summariseCohortCodeUse into a table.

Format the result of summariseCohortCodeUse into a table.

## Usage

``` r
tableCohortCodeUse(
  result,
  type = NULL,
  header = c("cdm_name", "estimate_name"),
  groupColumn = character(),
  hide = c("timing"),
  .options = list(),
  style = NULL
)
```

## Arguments

- result:

  A `<summarised_result>` with results of the type "cohort_code_use".

- type:

  Type of desired formatted table. By default (type = NULL), it will
  create a 'gt' table. To see supported formats use
  visOmopResults::tableType().

- header:

  A vector specifying the elements to include in the header. The order
  of elements matters, with the first being the topmost header. The
  header vector can contain one of the following variables: "cdm_name",
  "codelist_name", "standard_concept_name", "standard_concept_id",
  "estimate_name", "source_concept_name", "source_concept_id",
  "domain_id". If results are stratified, "year", "sex", "age_group" can
  also be used. Alternatively, it can include other names to use as
  overall header labels.

- groupColumn:

  Variables to use as group labels. Allowed columns are: "cdm_name",
  "codelist_name", "standard_concept_name", "standard_concept_id",
  "estimate_name", "source_concept_name", "source_concept_id",
  "domain_id". If results are stratified, "year", "sex", "age_group" can
  also be used. These cannot be used in header.

- hide:

  Table columns to exclude, options are: "cdm_name", "codelist_name",
  "year", "sex", "age_group", "standard_concept_name",
  "standard_concept_id", "estimate_name", "source_concept_name",
  "source_concept_id", "domain_id". If results are stratified, "year",
  "sex", "age_group" can also be used. These cannot be used in header or
  groupColumn.

- .options:

  Named list with additional formatting options.
  visOmopResults::tableOptions() shows allowed arguments and their
  default values.

- style:

  A character string or custom R code to define the visual formatting of
  the table. This argument can be provided in two ways: (1) Pre-defined
  Styles (Character String): Use a name for a built-in style (e.g.,
  "darwin"). See visOmopResults::tableStyle() for available options. (2)
  Custom Code (Advanced): Supply a block of custom R code. This code
  must be specific to the table type. See
  visOmopResults::tableStyleCode() for structural examples.

## Value

A table with a formatted version of the summariseCohortCodeUse result.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- DBI::dbConnect(duckdb::duckdb(),
                      dbdir = CDMConnector::eunomiaDir())
cdm <- CDMConnector::cdmFromCon(con,
                                  cdmSchema = "main",
                                  writeSchema = "main")
cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
conceptSet = list(a = 260139,
                  b = 1127433),
                  name = "cohorts",
                  end = "observation_period_end_date",
                  overwrite = TRUE)

results_cohort_mult <-
summariseCohortCodeUse(list(cs = c(260139,19133873)),
                      cdm = cdm,
                      cohortTable = "cohorts",
                      timing = "entry")

tableCohortCodeUse(results_cohort_mult)
CDMConnector::cdmDisconnect(cdm)
} # }
```
