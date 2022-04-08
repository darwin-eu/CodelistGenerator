
devtools::build_readme()
devtools::document()
devtools::spell_check()
devtools::test()

devtools::check()

# devtools::load_all()
# usethis::use_github_pages()
# usethis::use_pkgdown_github_pages()

# usethis::use_vignette("Introduction_to_CodelistGenerator")

# usethis::use_version()

# use_r("clean_words")
# use_r("get_candidate_codes")
# use_r("show_mappings")

# usethis::use_package("checkmate")
# usethis::use_package("dplyr")
# usethis::use_package("dtplyr")
# usethis::use_package("tidyr")
# usethis::use_package("stringr")

# use_mit_license()

# use_testthat()
# use_test("clean_words")
# usethis::use_test("show_mappings")

# use_readme_rmd()
#
# devtools::load_all()


# data for vignettes -----
# to avoid fails in continuous integration of vignettes
# (because of the use of external date)
# will create the data for vignettes here
library(readr)
library(DBI)
library(RSQLite)
library(here)
library(dplyr)
library(stringr)
library(DT)
library(kableExtra)
devtools::load_all()

vocab.folder<-Sys.getenv("omop_cdm_vocab_path") # path to directory of unzipped files
concept<-read_delim(paste0(vocab.folder,"/CONCEPT.csv"),
     "\t", escape_double = FALSE, trim_ws = TRUE)
concept_relationship<-read_delim(paste0(vocab.folder,"/CONCEPT_RELATIONSHIP.csv"),
     "\t", escape_double = FALSE, trim_ws = TRUE)
concept_ancestor<-read_delim(paste0(vocab.folder,"/CONCEPT_ANCESTOR.csv"),
     "\t", escape_double = FALSE, trim_ws = TRUE)
concept_synonym<-read_delim(paste0(vocab.folder,"/CONCEPT_SYNONYM.csv"),
     "\t", escape_double = FALSE, trim_ws = TRUE)

db <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(db, "concept", concept, overwrite=TRUE)
dbWriteTable(db, "concept_relationship", concept_relationship, overwrite=TRUE)
dbWriteTable(db, "concept_ancestor", concept_ancestor, overwrite=TRUE)
dbWriteTable(db, "concept_synonym", concept_synonym, overwrite=TRUE)
rm(concept,concept_relationship, concept_ancestor, concept_synonym)
vocabulary_database_schema<-"main"


codes_from_descendants<-tbl(db, sql(paste0("SELECT * FROM ",
     vocabulary_database_schema,
     ".concept_ancestor"))) %>%
  filter(ancestor_concept_id=="4182210") %>%
  select("descendant_concept_id") %>%
  rename("concept_id"="descendant_concept_id") %>%
  left_join(tbl(db, sql(paste0("SELECT * FROM ",
     vocabulary_database_schema,
     ".concept"))))%>%
  select("concept_id", "concept_name", "domain_id", "vocabulary_id") %>%
  collect()
saveRDS(codes_from_descendants,
        here("vignettes","intro_data_01.RData"))

dementia_codes1<-get_candidate_codes(keywords="dementia",
                    domains="Condition",
                    search.synonyms = FALSE,
                    fuzzy.match = FALSE,
                    exclude = NULL,
                    include.descendants = TRUE,
                    include.ancestor = FALSE,
                    db=db,
                    vocabulary_database_schema =  vocabulary_database_schema)
saveRDS(dementia_codes1,
        here("vignettes","intro_data_02.RData"))

code_comparison<-full_join(codes_from_descendants  %>% mutate(type1="4182210 and descendants"),
                     dementia_codes1  %>% mutate(type2="CodelistGenerator")) %>%
  mutate(type=ifelse(!is.na(type1) & !is.na(type2),
                     "Both",
              ifelse(!is.na(type1) & is.na(type2),
                     "Only 4182210 and descendants",
              ifelse(is.na(type1) & !is.na(type2),
                     "Only CodelistGenerator",
                     NA)))) %>%
  select(-c("type1","type2"))
saveRDS(code_comparison,
        here("vignettes","intro_data_03.RData"))

icd_mappings<-show_mappings(candidate_codelist=dementia_codes1,
source_vocabularies="ICD10CM",
                    db=db,
                    vocabulary_database_schema =  vocabulary_database_schema)
saveRDS(icd_mappings,
        here("vignettes","intro_data_04.RData"))

read_mappings<-show_mappings(candidate_codelist=dementia_codes1,
source_vocabularies="Read",
                    db=db,
                    vocabulary_database_schema =  vocabulary_database_schema)
saveRDS(read_mappings,
        here("vignettes","intro_data_05.RData"))




