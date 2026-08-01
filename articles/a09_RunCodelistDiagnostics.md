# Codelist diagnostics

This vignette presents a set of functions to explore the use of codes in
a codelist. We will cover the following key functions:

- [`summariseAchillesCodeUse()`](https://darwin-eu.github.io/CodelistGenerator/reference/summariseAchillesCodeUse.md):
  Summarises the code use using ACHILLES tables.
- [`summariseCodeUse()`](https://darwin-eu.github.io/CodelistGenerator/reference/summariseCodeUse.md):
  Summarises the code use in patient-level data.
- [`summariseOrphanCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/summariseOrphanCodes.md):
  Identifies orphan codes related to a codelist using ACHILLES tables.
- [`summariseCohortCodeUse()`](https://darwin-eu.github.io/CodelistGenerator/reference/summariseCohortCodeUse.md):
  Evaluates codelist usage within a cohort.

Let’s start by loading the required packages, connecting to a mock
database, and generating a codelist for example purposes. We’ll use
[`getCandidateCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/getCandidateCodes.md)
to find our codes.

``` r

library(DBI)
library(duckdb)
library(dplyr)
library(CDMConnector)
library(CodelistGenerator)
library(CohortConstructor)
library(omopgenerics)

# Connect to the database and create the cdm object
con <- dbConnect(duckdb(), 
                      eunomiaDir("synpuf-1k", "5.3"))
cdm <- cdmFromCon(con = con, 
                  cdmName = "Eunomia Synpuf",
                  cdmSchema   = "main",
                  writeSchema = "main", 
                  achillesSchema = "main")

# Create a codelist for depression
depression <- getCandidateCodes(cdm,
                                keywords = "depression")
depression <- newCodelist(list("depression" = depression$concept_id))
```

## Running diagnostics for a codelist

### Summarise code use using ACHILLES tables

This function uses ACHILLES summary tables to count the number of
records and persons associated with each concept in a codelist. Notice
that it requires that ACHILLES tables are available in the CDM.

``` r

achilles_code_use <- summariseAchillesCodeUse(depression, 
                                              cdm, 
                                              countBy = c("record", "person"))
```

From this, we will obtain a [summarised
result](https://darwin-eu.github.io/omopgenerics/articles/summarised_result.html)
object. We can easily visualise the results using
[`tableAchillesCodeUse()`](https://darwin-eu.github.io/CodelistGenerator/reference/tableAchillesCodeUse.md):

``` r

tableAchillesCodeUse(achilles_code_use,
                     type = "gt")
```

[TABLE]

Notice that concepts with zero counts will not appear in the result
table.

### Summarise code use using patient-level data

This function performs a similar task as above but directly queries
patient-level data, making it usable even if ACHILLES tables are not
available. It can be configured to stratify results by concept
(`byConcept`), by year (`byYear`), by sex (`bySex`), or by age group
(`byAgeGroup`). We can further specify a specific time period
(`dateRange`).

``` r

code_use <- summariseCodeUse(depression,
                             cdm,
                             countBy = c("record", "person"),
                             byYear  = FALSE,
                             bySex   = FALSE,
                             ageGroup =  list("<=50" = c(0,50), ">50" = c(51,Inf)),
                             dateRange = as.Date(c("2010-01-01", "2020-01-01")))

tableCodeUse(code_use, type = "gt")
```

[TABLE]

### Identify orphan codes

Orphan codes are concepts that might be related to our codelist but that
have not been included. It can be used to ensure that we have not missed
any important concepts. Notice that this function uses ACHILLES tables.

[`summariseOrphanCodes()`](https://darwin-eu.github.io/CodelistGenerator/reference/summariseOrphanCodes.md)
will look for descendants (via *concept_descendants* table), ancestors
(via *concept_ancestor* table), and concepts related to the codes
included in the codelist (via *concept_relationship* table).
Additionally, if the cdm contains PHOEBE tables (*concept_recommended*
table), they will also be used.

``` r

orphan <- summariseOrphanCodes(depression, cdm)
tableOrphanCodes(orphan, type = "gt")
```

[TABLE]

## Run diagnostics within a cohort

You can also evaluate how the codelist is used within a specific cohort.
First, we will define a cohort using the
[`conceptCohort()`](https://ohdsi.github.io/CohortConstructor/reference/conceptCohort.html)
function from CohortConstructor package.

``` r

cdm[["depression"]] <- conceptCohort(cdm, 
                                     conceptSet = depression, 
                                     name = "depression")
```

Then, we can summarise the code use within this cohort:

``` r

cohort_code_use <- summariseCohortCodeUse(cdm,
                                          cohortTable = "depression",
                                          countBy = c("record", "person"))
tableCohortCodeUse(cohort_code_use)
```

[TABLE]

### Summarise code use at cohort entry

Use the `timing` argument to restrict diagnostics to codes used at the
entry date of the cohort.

``` r

cohort_code_use <- summariseCohortCodeUse(cdm,
                                          cohortTable = "depression",
                                          countBy = c("record", "person"),
                                          timing = "entry")
tableCohortCodeUse(cohort_code_use)
```

[TABLE]

### Cohort code use with a different codelist

By default we’ll get cohort code use for the codes that were used for
creating the cohort. But we could change this to another cohort. Here
we’d get counts for anxiety codes that occur on the same day as entry
into the depression cohort.

``` r

anxiety <- getCandidateCodes(cdm,
                             keywords = "anxiety")
anxiety <- newCodelist(list("anxiety" = anxiety$concept_id))

cohort_code_use <- summariseCohortCodeUse(cdm,
                                          cohortTable = "depression",
                                          x = anxiety,
                                          countBy = c("record", "person"),
                                          timing = "entry")
tableCohortCodeUse(cohort_code_use)
```

[TABLE]

### Stratify cohort code use

You can also stratify cohort code use results by year (`byYear`), by sex
(`bySex`), or by age group (`byAgeGroup`):

``` r

cohort_code_use <- summariseCohortCodeUse(cdm = cdm,
                                          cohortTable = "depression",
                                          countBy = c("record", "person"),
                                          byYear = FALSE,
                                          bySex = TRUE,
                                          ageGroup = NULL)
tableCohortCodeUse(cohort_code_use)
```

[TABLE]
