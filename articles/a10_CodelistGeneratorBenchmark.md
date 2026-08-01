# Benchmarking the CodelistGenerator R package

To check the performance of the CodelistGenerator package, we can use
the benchmarkCodelistGenerator(). This function generates some
hypothetical study codelists using various settings and times to check
how long these analyses take.

We can start for example by benchmarking our mock data, which uses
duckdb.

``` r

library(DBI)
library(duckdb)
library(dplyr)
library(CDMConnector)
library(CodelistGenerator)
library(visOmopResults)

# Connect to the database and create the cdm object
con <- dbConnect(duckdb(), 
                      eunomiaDir("synpuf-1k", "5.3"))
cdm <- cdmFromCon(con = con, 
                  cdmName = "Eunomia Synpuf",
                  cdmSchema   = "main",
                  writeSchema = "main", 
                  achillesSchema = "main")

timings <- benchmarkCodelistGenerator(cdm)

visOmopTable(timings,
             hide = c("variable_name", "variable_level", "strata_name", "strata_level"),
             groupColumn = "task")
```

| Data source | Dbms | N person | N concepts | Estimate name | Estimate value |
|----|----|----|----|----|----|
| Getting drug ingredient codes (acetaminophen, codeine, adalimumab) |  |  |  |  |  |
| Eunomia Synpuf | duckdb | 1,000 | 6,224,227 | time_taken_minutes | 0.01 |
| Getting atc codes (atc 1st level, 1 name of interest) |  |  |  |  |  |
| Eunomia Synpuf | duckdb | 1,000 | 6,224,227 | time_taken_minutes | 0.09 |
| Getting candidate codes for dementia, excluding \`child\`, within all domains, and all options set to true |  |  |  |  |  |
| Eunomia Synpuf | duckdb | 1,000 | 6,224,227 | time_taken_minutes | 0.12 |
