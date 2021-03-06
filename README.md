
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![codecov.io](https://codecov.io/github/darwin-eu/CodelistGenerator/coverage.svg?branch=main)](https://codecov.io/github/darwin-eu/CodelistGenerator?branch=main)
[![R-CMD-check](https://github.com/darwin-eu/CodelistGenerator/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/CodelistGenerator/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

# CodelistGenerator

## Introduction

CodelistGenerator is used to create a candidate set of codes for helping
to define patient cohorts in data mapped to the OMOP common data model.
A little like the process for a systematic review, the idea is that for
a specified search strategy, CodelistGenerator will identify a set of
concepts that may be relevant, with these then being screened to remove
any irrelevant codes.

## Installation

You can install the development version of CodelistGenerator like so:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu/CodelistGenerator")
```

## Connecting to the OMOP CDM vocabularies

``` r
# First load required libraries
library(DBI)
library(dplyr)
library(dbplyr)
library(CodelistGenerator)

# Note that you will also need another library, like RPostgres, to make your database connection
```

``` r
# example with postgres database connection details
serverDbi<-Sys.getenv("server")
user<-Sys.getenv("user")
password<- Sys.getenv("password")
port<-Sys.getenv("port")
host<-Sys.getenv("host")

db <- DBI::dbConnect(RPostgres::Postgres(),
                dbname = serverDbi,
                port = port,
                host = host,
                user = user,
                password = password)

# name of vocabulary schema
vocabularyDatabaseSchema<-Sys.getenv("vocabulary_schema")
```

## Example search

Every codelist is specific to a version of the OMOP CDM vocabularies, so
we can first check the version.

``` r
dplyr::tbl(db, dplyr::sql(paste0(
    "SELECT * FROM ",
    vocabularyDatabaseSchema,
    ".vocabulary"
    ))) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::filter(.data$vocabulary_id == "None") %>%
    dplyr::select("vocabulary_version") %>%
    dplyr::collect() %>%
    dplyr::pull()
#> [1] "v5.0 13-JUL-21"
```

We can then search for asthma like so

``` r
asthma1<-getCandidateCodes(keywords="asthma",
                    domains = "Condition",
                    db=db,
                    vocabularyDatabaseSchema = vocabularyDatabaseSchema)
head(asthma1, 10)
```

<table>

<thead>

<tr>

<th style="text-align:right;">

concept\_id

</th>

<th style="text-align:left;">

concept\_name

</th>

<th style="text-align:left;">

domain\_id

</th>

<th style="text-align:left;">

concept\_class\_id

</th>

<th style="text-align:left;">

vocabulary\_id

</th>

<th style="text-align:left;">

found\_from

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

252658

</td>

<td style="text-align:left;">

Intrinsic asthma without status asthmaticus

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

252942

</td>

<td style="text-align:left;">

Asthmatic pulmonary eosinophilia

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

256448

</td>

<td style="text-align:left;">

Chronic asthmatic bronchitis

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

257581

</td>

<td style="text-align:left;">

Exacerbation of asthma

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

312950

</td>

<td style="text-align:left;">

IgE-mediated allergic asthma

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

313236

</td>

<td style="text-align:left;">

Cough variant asthma

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

316577

</td>

<td style="text-align:left;">

Poisoning by antiasthmatic

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

317009

</td>

<td style="text-align:left;">

Asthma

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

443801

</td>

<td style="text-align:left;">

Exercise-induced asthma

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

761844

</td>

<td style="text-align:left;">

Inhaled steroid-dependent asthma

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

</tbody>

</table>

Perhaps we want to exclude certain concepts as part of the search
strategy, in which case this can be added like so

``` r
asthma2<-getCandidateCodes(keywords="asthma",
                    domains = "Condition",
                    exclude = "Poisoning by antiasthmatic",
                    db=db,
                    vocabularyDatabaseSchema = vocabularyDatabaseSchema)
head(asthma2, 10)
```

<table>

<thead>

<tr>

<th style="text-align:right;">

concept\_id

</th>

<th style="text-align:left;">

concept\_name

</th>

<th style="text-align:left;">

domain\_id

</th>

<th style="text-align:left;">

concept\_class\_id

</th>

<th style="text-align:left;">

vocabulary\_id

</th>

<th style="text-align:left;">

found\_from

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

252658

</td>

<td style="text-align:left;">

Intrinsic asthma without status asthmaticus

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

252942

</td>

<td style="text-align:left;">

Asthmatic pulmonary eosinophilia

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

256448

</td>

<td style="text-align:left;">

Chronic asthmatic bronchitis

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

257581

</td>

<td style="text-align:left;">

Exacerbation of asthma

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

312950

</td>

<td style="text-align:left;">

IgE-mediated allergic asthma

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

313236

</td>

<td style="text-align:left;">

Cough variant asthma

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

317009

</td>

<td style="text-align:left;">

Asthma

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

443801

</td>

<td style="text-align:left;">

Exercise-induced asthma

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

761844

</td>

<td style="text-align:left;">

Inhaled steroid-dependent asthma

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

<tr>

<td style="text-align:right;">

764677

</td>

<td style="text-align:left;">

Persistent asthma

</td>

<td style="text-align:left;">

condition

</td>

<td style="text-align:left;">

clinical finding

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

From initial search

</td>

</tr>

</tbody>

</table>

We can then also see non-standard codes these are mapped from, for
example

``` r
asthmaIcdMappings<-showMappings(candidateCodelist=asthma2,
                     nonStandardVocabularies="ICD10CM",
                    db=db,
                    vocabularyDatabaseSchema =  vocabularyDatabaseSchema)
head(asthmaIcdMappings %>% 
       select(standard_concept_name,
              standard_vocabulary_id,
              non_standard_concept_name,
              non_standard_vocabulary_id),
     10)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

standard\_concept\_name

</th>

<th style="text-align:left;">

standard\_vocabulary\_id

</th>

<th style="text-align:left;">

non\_standard\_concept\_name

</th>

<th style="text-align:left;">

non\_standard\_vocabulary\_id

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Eosinophilic asthma

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

Pulmonary eosinophilia, not elsewhere classified

</td>

<td style="text-align:left;">

ICD10CM

</td>

</tr>

<tr>

<td style="text-align:left;">

Eosinophilic asthma

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

Eosinophilic asthma

</td>

<td style="text-align:left;">

ICD10CM

</td>

</tr>

<tr>

<td style="text-align:left;">

Eosinophilic asthma

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

Other pulmonary eosinophilia, not elsewhere classified

</td>

<td style="text-align:left;">

ICD10CM

</td>

</tr>

<tr>

<td style="text-align:left;">

Eosinophilic asthma

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

Pulmonary eosinophilia, not elsewhere classified

</td>

<td style="text-align:left;">

ICD10CM

</td>

</tr>

<tr>

<td style="text-align:left;">

Cryptogenic pulmonary eosinophilia

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

Chronic eosinophilic pneumonia

</td>

<td style="text-align:left;">

ICD10CM

</td>

</tr>

<tr>

<td style="text-align:left;">

Simple pulmonary eosinophilia

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

Acute eosinophilic pneumonia

</td>

<td style="text-align:left;">

ICD10CM

</td>

</tr>

<tr>

<td style="text-align:left;">

Asthma

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

Asthma

</td>

<td style="text-align:left;">

ICD10CM

</td>

</tr>

<tr>

<td style="text-align:left;">

Asthma

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

Other and unspecified asthma

</td>

<td style="text-align:left;">

ICD10CM

</td>

</tr>

<tr>

<td style="text-align:left;">

Asthma

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

Unspecified asthma

</td>

<td style="text-align:left;">

ICD10CM

</td>

</tr>

<tr>

<td style="text-align:left;">

Asthma

</td>

<td style="text-align:left;">

SNOMED

</td>

<td style="text-align:left;">

Other asthma

</td>

<td style="text-align:left;">

ICD10CM

</td>

</tr>

</tbody>

</table>
