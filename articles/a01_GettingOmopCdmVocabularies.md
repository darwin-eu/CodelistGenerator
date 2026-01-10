# Getting the OMOP CDM vocabularies

When working with the CodelistGenerator we normally have two options of
how to interact with the OMOP CDM vocabulary tables.

The first is to connect to a “live” database with patient data in the
OMOP CDM format. As part of this OMOP CDM dataset we will have a version
of vocabularies that corresponds to the concepts being used in the
patient records we have in the various clinical tables. This is useful
in that we will be working with the same vocabularies that are being
used for clinical records in this dataset. However, if working on a
study with multiple data partners we should take note that other data
partners may be using different vocabulary versions.

The second option is to create a standalone database with just a set of
OMOP CDM vocabulary tables. This is convenient because we can choose
whichever version and vocabularies we want. However, we will need to
keep in mind that this can differ to the version used for a particular
dataset.

## Connect to an existing OMOP CDM database

If you already have access to a database with data in the OMOP CDM
format, you can use CodelistGenerator by first creating a cdm reference
which will include the vocabulary tables.

``` r
library(DBI)
library(duckdb)
library(dplyr)
library(CDMConnector)
library(CodelistGenerator)
```

``` r
requireEunomia()
#> ℹ `EUNOMIA_DATA_FOLDER` set to: /tmp/RtmpDD9K6R.
#> 
#> Download completed!
db <- dbConnect(duckdb(), dbdir = eunomiaDir())
#> Creating CDM database /tmp/RtmpDD9K6R/GiBleed_5.3.zip
cdm <- cdmFromCon(db, 
                  cdmSchema = "main", 
                  writeSchema = "main", 
                  writePrefix = "cg_")
cdm
#> 
#> ── # OMOP CDM reference (duckdb) of Synthea ────────────────────────────────────
#> • omop tables: care_site, cdm_source, concept, concept_ancestor, concept_class,
#> concept_relationship, concept_synonym, condition_era, condition_occurrence,
#> cost, death, device_exposure, domain, dose_era, drug_era, drug_exposure,
#> drug_strength, fact_relationship, location, measurement, metadata, note,
#> note_nlp, observation, observation_period, payer_plan_period, person,
#> procedure_occurrence, provider, relationship, source_to_concept_map, specimen,
#> visit_detail, visit_occurrence, vocabulary
#> • cohort tables: -
#> • achilles tables: -
#> • other tables: -
```

We can see that we know have various OMOP CDM vocabulary tables we can
work with.

``` r
cdm$concept |> glimpse()
#> Rows: ??
#> Columns: 10
#> Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpDD9K6R/file226f75ec9ced.duckdb]
#> $ concept_id       <int> 35208414, 1118088, 40213201, 1557272, 4336464, 429588…
#> $ concept_name     <chr> "Gastrointestinal hemorrhage, unspecified", "celecoxi…
#> $ domain_id        <chr> "Condition", "Drug", "Drug", "Drug", "Procedure", "Pr…
#> $ vocabulary_id    <chr> "ICD10CM", "RxNorm", "CVX", "RxNorm", "SNOMED", "SNOM…
#> $ concept_class_id <chr> "4-char billing code", "Branded Drug", "CVX", "Ingred…
#> $ standard_concept <chr> NA, "S", "S", "S", "S", "S", "S", "S", NA, NA, "S", "…
#> $ concept_code     <chr> "K92.2", "213469", "33", "46041", "232717009", "76601…
#> $ valid_start_date <date> 2007-01-01, 1970-01-01, 2008-12-01, 1970-01-01, 1970…
#> $ valid_end_date   <date> 2099-12-31, 2099-12-31, 2099-12-31, 2099-12-31, 2099…
#> $ invalid_reason   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
cdm$concept_relationship |> glimpse()
#> Rows: ??
#> Columns: 6
#> Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpDD9K6R/file226f75ec9ced.duckdb]
#> $ concept_id_1     <int> 192671, 1118088, 1569708, 35208414, 35208414, 4016235…
#> $ concept_id_2     <int> 35208414, 44923712, 35208414, 192671, 1569708, 450118…
#> $ relationship_id  <chr> "Mapped from", "Mapped from", "Subsumes", "Maps to", …
#> $ valid_start_date <date> 1970-01-01, 1970-01-01, 2016-03-25, 1970-01-01, 2016…
#> $ valid_end_date   <date> 2099-12-31, 2099-12-31, 2099-12-31, 2099-12-31, 2099…
#> $ invalid_reason   <chr> NA, NA, NA, NA, NA, NA, NA, NA
cdm$concept_ancestor |> glimpse()
#> Rows: ??
#> Columns: 4
#> Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpDD9K6R/file226f75ec9ced.duckdb]
#> $ ancestor_concept_id      <int> 4180628, 4179141, 21500574, 21505770, 2150396…
#> $ descendant_concept_id    <int> 313217, 4146173, 1118084, 1119510, 40162522, …
#> $ min_levels_of_separation <int> 5, 2, 4, 0, 5, 4, 0, 4, 2, 2, 0, 0, 0, 0, 0, …
#> $ max_levels_of_separation <int> 6, 2, 4, 0, 6, 4, 0, 4, 2, 2, 0, 0, 0, 0, 0, …
cdm$concept_synonym |> glimpse()
#> Rows: ??
#> Columns: 3
#> Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpDD9K6R/file226f75ec9ced.duckdb]
#> $ concept_id           <int> 964261, 1322184, 441267, 1718412, 4336464, 410212…
#> $ concept_synonym_name <chr> "cyanocobalamin 5000 MCG/ML Injectable Solution",…
#> $ language_concept_id  <int> 4180186, 4180186, 4180186, 4180186, 4180186, 4180…
cdm$drug_strength |> glimpse()
#> Rows: ??
#> Columns: 12
#> Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2//tmp/RtmpDD9K6R/file226f75ec9ced.duckdb]
#> $ drug_concept_id             <int> 
#> $ ingredient_concept_id       <int> 
#> $ amount_value                <dbl> 
#> $ amount_unit_concept_id      <int> 
#> $ numerator_value             <dbl> 
#> $ numerator_unit_concept_id   <int> 
#> $ denominator_value           <dbl> 
#> $ denominator_unit_concept_id <int> 
#> $ box_size                    <int> 
#> $ valid_start_date            <date> 
#> $ valid_end_date              <date> 
#> $ invalid_reason              <chr>
```

It is important to remember that our results will be tied to the
vocabulary version used when this OMOP CDM database was created.
Moreover, we should also take note of which vocabularies were included.
A couple of CodelistGenerator utility functions can help us find this
information.

``` r
vocabularyVersion(cdm)
#> [1] "v5.0 18-JAN-19"
```

``` r
availableVocabularies(cdm)
#> [1] "CVX"    "Gender" "LOINC"  "RxNorm" "SNOMED" "Visit"
```

## Create a local vocabulary database

If you don’t have access to an OMOP CDM database or if you want to work
with a specific vocabulary version and set of vocabularies then you can
create your own vocabulary database.

### Download vocabularies from athena

Your first step will be to get the vocabulary tables for the OMOP CDM.
For this go to the OHDSI ATHENA website (athena.ohdsi.org). From here
you can, after creating a free account, download the vocabularies. By
default you will be getting the latest version and a default set of
vocabularies. You can though choose to download an older version and
expand your selection of vocabularies. In general we would suggest to
select all available vocabularies.

### Create a duckdb database

After downloading the vocabularies you will have a set of csvs (along
with a tool to add the CPT-4 codes if you wish). To quickly create a
duckdb vocab database you could use the following code. Here, after
pointing to the unzipped folder containg the csvs, we’ll read each table
into memory and write them to a duckdb database which we’ll save in the
same folder. We’ll also add an empty person and observation period table
so that you can create a cdm reference at the end.

``` r
library(readr)
library(DBI)
library(duckdb)
library(omopgenerics)
library(here)

vocab_folder <- here() # add path to directory

# read in files
concept <- read_delim(here(vocab_folder, "CONCEPT.csv"),
                      "\t",
                      escape_double = FALSE, trim_ws = TRUE
)
concept_relationship <- read_delim(here(vocab_folder, "CONCEPT_RELATIONSHIP.csv"),
                                   "\t",
                                   escape_double = FALSE, trim_ws = TRUE
)
concept_ancestor <- read_delim(here(vocab_folder, "CONCEPT_ANCESTOR.csv"),
                               "\t",
                               escape_double = FALSE, trim_ws = TRUE
)
concept_synonym <- read_delim(here(vocab_folder, "CONCEPT_SYNONYM.csv"),
                              "\t",
                              escape_double = FALSE, trim_ws = TRUE
)
vocabulary <- read_delim(here(vocab_folder, "VOCABULARY.csv"), "\t",
                         escape_double = FALSE, trim_ws = TRUE
)

# write to duckdb
db <- dbConnect(duckdb(), here(vocab_folder,"vocab.duckdb"))
dbWriteTable(db, "concept", concept, overwrite = TRUE)
dbWriteTable(db, "concept_relationship", concept_relationship, overwrite = TRUE)
dbWriteTable(db, "concept_ancestor", concept_ancestor, overwrite = TRUE)
dbWriteTable(db, "concept_synonym", concept_synonym, overwrite = TRUE)
dbWriteTable(db, "vocabulary", vocabulary, overwrite = TRUE)
# add empty person and observation period tables
person_cols <- omopColumns("person")
person <- data.frame(matrix(ncol = length(person_cols), nrow = 0))
colnames(person) <- person_cols
dbWriteTable(db, "person", person, overwrite = TRUE)
observation_period_cols <- omopColumns("observation_period")
observation_period <- data.frame(matrix(ncol = length(observation_period_cols), nrow = 0))
colnames(observation_period) <- observation_period_cols
dbWriteTable(db, "observation_period", observation_period, overwrite = TRUE)
dbDisconnect(db)
```

Now we could create a cdm reference to our OMOP CDM vocabulary database.

``` r
db <- dbConnect(duckdb(), here(vocab_folder,"vocab.duckdb"))
cdm <- cdmFromCon(db, "main", "main", cdmName = "vocabularise", .softValidation = TRUE)
```

This vocabulary only database can be then used for the various functions
for identifying codes of interest. However, as it doesn’t contain
patient-level records it won’t be relevant for functions summarising the
use of codes, etc. Here we have shown how to make a local duckdb
database, but a similar approach could also be used for other database
management systems.
