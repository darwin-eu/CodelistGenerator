# Build local vocabulary database
library(readr)
library(DBI)
library(RSQLite)
library(here)

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
vocabulary <- read_delim(paste0(vocab.folder, "/VOCABULARY.csv"), "\t",
  escape_double = FALSE, trim_ws = TRUE
)

db <- dbConnect(RSQLite::SQLite(), paste0(Sys.getenv("omop_cdm_vocab_path"),".sqlite"))
dbWriteTable(db, "concept", concept, overwrite = TRUE)
dbWriteTable(db, "concept_relationship", concept_relationship, overwrite = TRUE)
dbWriteTable(db, "concept_ancestor", concept_ancestor, overwrite = TRUE)
dbWriteTable(db, "concept_synonym", concept_synonym, overwrite = TRUE)
dbWriteTable(db, "vocabulary", vocabulary)
dbExecute(db, "CREATE UNIQUE INDEX idx_concept_concept_id ON concept (concept_id)")
dbExecute(db, "CREATE INDEX idx_concept_relationship_id_1 ON concept_relationship (concept_id_1, concept_id_2)")
dbExecute(db, "CREATE INDEX idx_concept_ancestor_id_1 ON concept_ancestor (ancestor_concept_id)")
dbExecute(db, "CREATE INDEX idx_concept_ancestor_id_2 ON concept_ancestor (descendant_concept_id)")
dbExecute(db, "CREATE INDEX idx_concept_synonym_id ON concept_synonym (concept_id)")
rm(concept, concept_relationship, concept_ancestor, concept_synonym)
dbDisconnect(db)
