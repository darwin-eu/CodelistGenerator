# precompute data for vignettes -----
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

vocab.folder <- Sys.getenv("omop_cdm_vocab_path") # path to directory of unzipped files
concept <- read_delim(paste0(vocab.folder, "/CONCEPT.csv"),
  "\t",
  escape_double = FALSE, trim_ws = TRUE
)
concept_relationship <- read_delim(paste0(vocab.folder, "/CONCEPT_RELATIONSHIP.csv"),
  "\t",
  escape_double = FALSE, trim_ws = TRUE
)
concept_ancestor <- read_delim(paste0(vocab.folder, "/CONCEPT_ANCESTOR.csv"),
  "\t",
  escape_double = FALSE, trim_ws = TRUE
)
concept_synonym <- read_delim(paste0(vocab.folder, "/CONCEPT_SYNONYM.csv"),
  "\t",
  escape_double = FALSE, trim_ws = TRUE
)
vocabulary <- read_delim(paste0(vocab.folder, "/VOCABULARY.csv"),
  "\t",
  escape_double = FALSE, trim_ws = TRUE
)

db <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(db, "concept", concept, overwrite = TRUE)
dbWriteTable(db, "concept_relationship", concept_relationship, overwrite = TRUE)
dbWriteTable(db, "concept_ancestor", concept_ancestor, overwrite = TRUE)
dbWriteTable(db, "concept_synonym", concept_synonym, overwrite = TRUE)
dbWriteTable(db, "vocabulary", vocabulary)
rm(concept, concept_relationship, concept_ancestor, concept_synonym)
vocabulary_database_schema <- "main"

# for intro vignette
vocab_version <- get_vocab_version(
  db = db,
  vocabulary_database_schema = "main"
)
saveRDS(
  vocab_version,
  here("vignettes", "intro_vocab.RData")
)

codes_from_descendants <- tbl(db, sql(paste0(
  "SELECT * FROM ",
  vocabulary_database_schema,
  ".concept_ancestor"
))) %>%
  filter(ancestor_concept_id == "4182210") %>%
  select("descendant_concept_id") %>%
  rename("concept_id" = "descendant_concept_id") %>%
  left_join(tbl(db, sql(paste0(
    "SELECT * FROM ",
    vocabulary_database_schema,
    ".concept"
  )))) %>%
  select("concept_id", "concept_name", "domain_id", "vocabulary_id") %>%
  collect()
saveRDS(
  codes_from_descendants,
  here("vignettes", "intro_data_01.RData")
)

dementia_codes1 <- get_candidate_codes(
  keywords = "dementia",
  domains = "Condition",
  search_synonyms = FALSE,
  fuzzy_match = FALSE,
  exclude = NULL,
  include_descendants = TRUE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  dementia_codes1,
  here("vignettes", "intro_data_02.RData")
)

code_comparison <- full_join(
  codes_from_descendants %>% mutate(type1 = "4182210 and descendants"),
  dementia_codes1 %>% mutate(type2 = "CodelistGenerator")
) %>%
  mutate(type = ifelse(!is.na(type1) & !is.na(type2),
    "Both",
    ifelse(!is.na(type1) & is.na(type2),
      "Only 4182210 and descendants",
      ifelse(is.na(type1) & !is.na(type2),
        "Only CodelistGenerator",
        NA
      )
    )
  )) %>%
  select(-c("type1", "type2"))
saveRDS(
  code_comparison,
  here("vignettes", "intro_data_03.RData")
)

icd_mappings <- show_mappings(
  candidate_codelist = dementia_codes1,
  source_vocabularies = "ICD10CM",
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  icd_mappings,
  here("vignettes", "intro_data_04.RData")
)

read_mappings <- show_mappings(
  candidate_codelist = dementia_codes1,
  source_vocabularies = "Read",
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  read_mappings,
  here("vignettes", "intro_data_05.RData")
)


# for options vignette
oa_codes1 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = "Condition",
  search_synonyms = FALSE,
  fuzzy_match = FALSE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = FALSE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes1,
  here("vignettes", "options_data_01.RData")
)

# include desc
oa_codes2 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = "Condition",
  search_synonyms = FALSE,
  fuzzy_match = FALSE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = TRUE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes2,
  here("vignettes", "options_data_02.RData")
)

# include obs
oa_codes3 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = c("Condition", "Observation"),
  search_synonyms = FALSE,
  fuzzy_match = FALSE,
                    max_distance_substitutions = 0.1,
                    max_distance_deletions = 0.1,
                    max_distance_insertions = 0.1,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = FALSE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes3,
  here("vignettes", "options_data_03.RData")
)

# search syn
oa_codes4 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = "Condition",
  search_synonyms = TRUE,
  fuzzy_match = FALSE,
                    max_distance_substitutions = 0.1,
                    max_distance_deletions = 0.1,
                    max_distance_insertions = 0.1,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = FALSE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes4,
  here("vignettes", "options_data_04.RData")
)

# fuzzy search
oa_codes5 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = "Condition",
  search_synonyms = FALSE,
  fuzzy_match = TRUE,
                    max_distance_substitutions = 0.1,
                    max_distance_deletions = 0.1,
                    max_distance_insertions = 0.1,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = FALSE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes5,
  here("vignettes", "options_data_05.RData")
)

# fuzzy search 0.2
oa_codes6 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = "Condition",
  search_synonyms = FALSE,
  fuzzy_match = TRUE,
                    max_distance_substitutions = 0.2,
                    max_distance_deletions = 0.2,
                    max_distance_insertions = 0.2,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = FALSE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes6,
  here("vignettes", "options_data_06.RData")
)

# include ancestor
oa_codes7 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = "Condition",
  search_synonyms = FALSE,
  fuzzy_match = FALSE,
                    max_distance_substitutions = 0.2,
                    max_distance_deletions = 0.2,
                    max_distance_insertions = 0.2,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = FALSE,
  include_ancestor = TRUE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes7,
  here("vignettes", "options_data_07.RData")
)
