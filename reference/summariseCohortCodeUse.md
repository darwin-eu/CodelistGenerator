# Summarise code use among a cohort in the cdm reference

Summarise code use among a cohort in the cdm reference

## Usage

``` r
summariseCohortCodeUse(
  cdm,
  cohortTable,
  x = NULL,
  cohortId = NULL,
  timing = "any",
  countBy = c("record", "person"),
  byConcept = TRUE,
  byYear = FALSE,
  bySex = FALSE,
  ageGroup = NULL,
  useSourceCodes = FALSE
)
```

## Arguments

- cdm:

  A cdm reference to an OMOP CDM dataset. If data is held within a
  database, the vocabulary tables should be in the same schema as the
  clinical tables (person, observation period, and so on).

- cohortTable:

  A cohort table from the cdm reference.

- x:

  A codelist or cohort table name.

- cohortId:

  A vector of cohort IDs to include

- timing:

  When to assess the code use relative cohort dates. This can be
  "any"(code use any time by individuals in the cohort) or "entry" (code
  use on individuals' cohort start date).

- countBy:

  Either "record" for record-level counts or "person" for person-level
  counts.

- byConcept:

  TRUE or FALSE. If TRUE code use will be summarised by concept.

- byYear:

  TRUE or FALSE. If TRUE code use will be summarised by year.

- bySex:

  TRUE or FALSE. If TRUE code use will be summarised by sex.

- ageGroup:

  If not NULL, a list of ageGroup vectors of length two.

- useSourceCodes:

  Whether the codelist provided contains source codes (TRUE) or standard
  codes (FALSE).

## Value

A tibble with results overall and, if specified, by strata

## Examples

``` r
if (FALSE) { # \dontrun{
library(CodelistGenerator)
library(duckdb)
library(DBI)
library(CDMConnector)
con <- dbConnect(duckdb(),
                 dbdir = eunomiaDir())
cdm <- cdmFromCon(con,
                 cdmSchema = "main",
                 writeSchema = "main")
cdm <- generateConceptCohortSet(cdm = cdm,
                  conceptSet = list(a = 260139,
                                    b = 1127433),
                  name = "cohorts",
                  end = "observation_period_end_date",
                  overwrite = TRUE)

results_cohort_mult <-
summariseCohortCodeUse(omopgenerics::newCodelist(list(cs = c(260139,19133873))),
                      cdm = cdm,
                      cohortTable = "cohorts",
                      timing = "entry")

results_cohort_mult
CDMConnector::cdmDisconnect(cdm)
} # }
```
