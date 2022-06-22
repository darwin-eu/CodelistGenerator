
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
# example with postgres database connection details
server_dbi<-Sys.getenv("server")
user<-Sys.getenv("user")
password<- Sys.getenv("password")
port<-Sys.getenv("port")
host<-Sys.getenv("host")

db <- DBI::dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password)

# name of vocabulary schema
vocabulary_database_schema<-Sys.getenv("vocabulary_schema")
```

## Example search

Every codelist is specific to a version of the OMOP CDM vocabularies, so
we can first check the version.

``` r
dplyr::tbl(db, dplyr::sql(paste0(
    "SELECT * FROM ",
    vocabulary_database_schema,
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
asthma_1<-get_candidate_codes(keywords="asthma",
                    domains = "Condition",
                    db=db,
                    vocabulary_database_schema = vocabulary_database_schema)
kable(head(asthma_1, 10))
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

vocabulary\_id

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

761844

</td>

<td style="text-align:left;">

Inhaled steroid-dependent asthma

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

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

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

764949

</td>

<td style="text-align:left;">

Persistent asthma, well controlled

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

3661412

</td>

<td style="text-align:left;">

Thunderstorm asthma

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

4015819

</td>

<td style="text-align:left;">

Asthma disturbs sleep weekly

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

4015947

</td>

<td style="text-align:left;">

Asthma causing night waking

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

4017025

</td>

<td style="text-align:left;">

Asthma disturbing sleep

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

4017026

</td>

<td style="text-align:left;">

Asthma not limiting activities

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

4017182

</td>

<td style="text-align:left;">

Asthma disturbs sleep frequently

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

4017183

</td>

<td style="text-align:left;">

Asthma not disturbing sleep

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

</tbody>

</table>

Perhaps we want to exclude certain concepts as part of the search
strategy, in which case this can be added like so

``` r
asthma_2<-get_candidate_codes(keywords="asthma",
                    domains = "Condition",
                    exclude = "Poisoning by antiasthmatic",
                    db=db,
                    vocabulary_database_schema = vocabulary_database_schema)
kable(head(asthma_2, 10))
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

vocabulary\_id

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

761844

</td>

<td style="text-align:left;">

Inhaled steroid-dependent asthma

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

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

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

764949

</td>

<td style="text-align:left;">

Persistent asthma, well controlled

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

3661412

</td>

<td style="text-align:left;">

Thunderstorm asthma

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

4015819

</td>

<td style="text-align:left;">

Asthma disturbs sleep weekly

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

4015947

</td>

<td style="text-align:left;">

Asthma causing night waking

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

4017025

</td>

<td style="text-align:left;">

Asthma disturbing sleep

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

4017026

</td>

<td style="text-align:left;">

Asthma not limiting activities

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

4017182

</td>

<td style="text-align:left;">

Asthma disturbs sleep frequently

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

<tr>

<td style="text-align:right;">

4017183

</td>

<td style="text-align:left;">

Asthma not disturbing sleep

</td>

<td style="text-align:left;">

Condition

</td>

<td style="text-align:left;">

SNOMED

</td>

</tr>

</tbody>

</table>

We can then also see source codes these are mapped from, for example

``` r
asthma_icd_mappings<-show_mappings(candidate_codelist=asthma_2,
                     source_vocabularies="ICD10CM",
                    db=db,
                    vocabulary_database_schema =  vocabulary_database_schema)
kable(head(asthma_icd_mappings %>% 
       select(standard_concept_name,
              standard_vocabulary_id,
              source_concept_name,
              source_vocabulary_id),
     10))
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

source\_concept\_name

</th>

<th style="text-align:left;">

source\_vocabulary\_id

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
