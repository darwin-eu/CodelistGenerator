
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

### Option 1: Connect to a live OMOP CDM database

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

### Option 2: Download the vocabularies from Athena

You will first need to obtain the OMOP CDM vocabularies from
<https://athena.ohdsi.org>. Once these are downloaded, you can make a
vocabulary only SQLite database like so:

``` r
vocab.folder<-Sys.getenv("omop_cdm_vocab_path") # path to directory of unzipped files
concept<-read_delim(paste0(vocab.folder,"/CONCEPT.csv"),
     "\t", escape_double = FALSE, trim_ws = TRUE)
concept_relationship<-read_delim(paste0(vocab.folder,"/CONCEPT_RELATIONSHIP.csv"),
     "\t", escape_double = FALSE, trim_ws = TRUE) 
concept_ancestor<-read_delim(paste0(vocab.folder,"/CONCEPT_ANCESTOR.csv"),
     "\t", escape_double = FALSE, trim_ws = TRUE)
concept_synonym<-read_delim(paste0(vocab.folder,"/CONCEPT_SYNONYM.csv"),
     "\t", escape_double = FALSE, trim_ws = TRUE)
vocabulary<-read_delim(paste0(vocab.folder,"/VOCABULARY.csv"),
     "\t", escape_double = FALSE, trim_ws = TRUE)

db <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(db, "concept", concept, overwrite=TRUE)
dbWriteTable(db, "concept_relationship", concept_relationship, overwrite=TRUE)
dbWriteTable(db, "concept_ancestor", concept_ancestor, overwrite=TRUE)
dbWriteTable(db, "concept_synonym", concept_synonym, overwrite=TRUE)
dbWriteTable(db, "vocabulary", vocabulary)
rm(concept,concept_relationship, concept_ancestor, concept_synonym, vocabulary)

vocabulary_database_schema<-"main"
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
head(asthma_1, 10)
#> # A tibble: 10 x 4
#>    concept_id concept_name                               domain_id vocabulary_id
#>         <int> <chr>                                      <chr>     <chr>        
#>  1     252658 Intrinsic asthma without status asthmatic~ Condition SNOMED       
#>  2     252942 Asthmatic pulmonary eosinophilia           Condition SNOMED       
#>  3     256448 Chronic asthmatic bronchitis               Condition SNOMED       
#>  4     257581 Exacerbation of asthma                     Condition SNOMED       
#>  5     312950 IgE-mediated allergic asthma               Condition SNOMED       
#>  6     313236 Cough variant asthma                       Condition SNOMED       
#>  7     316577 Poisoning by antiasthmatic                 Condition SNOMED       
#>  8     317009 Asthma                                     Condition SNOMED       
#>  9     443801 Exercise-induced asthma                    Condition SNOMED       
#> 10     761844 Inhaled steroid-dependent asthma           Condition SNOMED
```

Perhaps we want to exclude certain concepts as part of the search
strategy, in which case this can be added like so

``` r
asthma_2<-get_candidate_codes(keywords="asthma",
                    domains = "Condition",
                    exclude = "Poisoning by antiasthmatic",
                    db=db,
                    vocabulary_database_schema = vocabulary_database_schema)
head(asthma_2, 10)
#> # A tibble: 10 x 4
#>    concept_id concept_name                               domain_id vocabulary_id
#>         <int> <chr>                                      <chr>     <chr>        
#>  1     252658 Intrinsic asthma without status asthmatic~ Condition SNOMED       
#>  2     252942 Asthmatic pulmonary eosinophilia           Condition SNOMED       
#>  3     256448 Chronic asthmatic bronchitis               Condition SNOMED       
#>  4     257581 Exacerbation of asthma                     Condition SNOMED       
#>  5     312950 IgE-mediated allergic asthma               Condition SNOMED       
#>  6     313236 Cough variant asthma                       Condition SNOMED       
#>  7     317009 Asthma                                     Condition SNOMED       
#>  8     443801 Exercise-induced asthma                    Condition SNOMED       
#>  9     761844 Inhaled steroid-dependent asthma           Condition SNOMED       
#> 10     764677 Persistent asthma                          Condition SNOMED
```

We can then also see source codes these are mapped from, for example

``` r
asthma_icd_mappings<-show_mappings(candidate_codelist=asthma_2,
                     source_vocabularies="ICD10CM",
                    db=db,
                    vocabulary_database_schema =  vocabulary_database_schema)
head(asthma_icd_mappings %>% 
       select("Standard concept_id name",
              "Source name" ),
     10)
#> # A tibble: 10 x 2
#>    `Standard concept_id name`         `Source name`                             
#>    <chr>                              <chr>                                     
#>  1 Eosinophilic asthma                Pulmonary eosinophilia, not elsewhere cla~
#>  2 Eosinophilic asthma                Eosinophilic asthma                       
#>  3 Eosinophilic asthma                Other pulmonary eosinophilia, not elsewhe~
#>  4 Eosinophilic asthma                Pulmonary eosinophilia, not elsewhere cla~
#>  5 Cryptogenic pulmonary eosinophilia Chronic eosinophilic pneumonia            
#>  6 Simple pulmonary eosinophilia      Acute eosinophilic pneumonia              
#>  7 Asthma                             Asthma                                    
#>  8 Asthma                             Other and unspecified asthma              
#>  9 Asthma                             Unspecified asthma                        
#> 10 Asthma                             Other asthma

dbDisconnect(db)
```
