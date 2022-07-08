## precompute data for vignettes -----
# to avoid fails in continuous integration of vignettes
# (because of the use of external data)
# will create the data for vignettes here
library(readr)
library(DBI)
library(RSQLite)
library(here)
library(dplyr)
library(dbplyr)
library(stringr)
library(DT)
library(kableExtra)
devtools::load_all()

db<-dbConnect(RSQLite::SQLite(), paste0(Sys.getenv("omop_cdm_vocab_path"),".sqlite"))
vocabularyDatabaseSchema <- "main"



# intro vignette ----
vocabVersion <- dplyr::tbl(db, dplyr::sql(paste0(
    "SELECT * FROM ",
    vocabularyDatabaseSchema,
    ".vocabulary"
    ))) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::filter(.data$vocabulary_id == "None") %>%
    dplyr::select("vocabulary_version") %>%
    dplyr::collect() %>%
    dplyr::pull()
saveRDS(
  vocabVersion,
  here("vignettes", "introVocab.RData")
)

codesFromDescendants <- tbl(db, sql(paste0(
  "SELECT * FROM ",
  vocabularyDatabaseSchema,
  ".concept_ancestor"
))) %>%
  filter(ancestor_concept_id == "4182210") %>%
  select("descendant_concept_id") %>%
  rename("concept_id" = "descendant_concept_id") %>%
  left_join(tbl(db, sql(paste0(
    "SELECT * FROM ",
    vocabularyDatabaseSchema,
    ".concept"
  )))) %>%
  select("concept_id", "concept_name", "domain_id", "vocabulary_id") %>%
  collect()
saveRDS(
  codesFromDescendants,
  here("vignettes", "introData01.RData")
)

dementiaCodes1 <- getCandidateCodes(
  keywords = "dementia",
  domains = "Condition",
  searchSynonyms = FALSE,
  fuzzyMatch = FALSE,
  exclude = NULL,
  includeDescendants = TRUE,
  includeAncestor = FALSE,
  db = db,
  vocabularyDatabaseSchema = vocabularyDatabaseSchema
)
saveRDS(
  dementiaCodes1,
  here("vignettes", "introData02.RData")
)

codeComparison <- compareCodelists(codesFromDescendants,
                  dementiaCodes1)
saveRDS(
  codeComparison,
  here("vignettes", "introData03.RData")
)

icdMappings <- showMappings(
  candidateCodelist = dementiaCodes1,
  sourceVocabularies = "ICD10CM",
  db = db,
  vocabularyDatabaseSchema = vocabularyDatabaseSchema
)
saveRDS(
  icdMappings,
  here("vignettes", "introData04.RData")
)

readMappings <- showMappings(
  candidateCodelist = dementiaCodes1,
  sourceVocabularies = "Read",
  db = db,
  vocabularyDatabaseSchema = vocabularyDatabaseSchema
)
saveRDS(
  readMappings,
  here("vignettes", "introData05.RData")
)


# options vignette ------
oaCodes1 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = "Condition",
  searchSynonyms = FALSE,
  fuzzyMatch = FALSE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE,
  db = db,
  vocabularyDatabaseSchema = vocabularyDatabaseSchema
)
saveRDS(
  oaCodes1,
  here("vignettes", "optionsData01.RData")
)

# include desc
oaCodes2 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = "Condition",
  searchSynonyms = FALSE,
  fuzzyMatch = FALSE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = TRUE,
  includeAncestor = FALSE,
  db = db,
  vocabularyDatabaseSchema = vocabularyDatabaseSchema
)
saveRDS(
  oaCodes2,
  here("vignettes", "optionsData02.RData")
)

# include obs
oaCodes3 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = c("Condition", "Observation"),
  searchSynonyms = FALSE,
  fuzzyMatch = FALSE,
  maxDistanceCost = 0.1,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE,
  db = db,
  vocabularyDatabaseSchema = vocabularyDatabaseSchema
)
saveRDS(
  oaCodes3,
  here("vignettes", "optionsData03.RData")
)

# search syn
oaCodes4 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = "Condition",
  searchSynonyms = TRUE,
  fuzzyMatch = FALSE,
  maxDistanceCost = 0.1,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE,
  db = db,
  vocabularyDatabaseSchema = vocabularyDatabaseSchema
)
saveRDS(
  oaCodes4,
  here("vignettes", "optionsData04.RData")
)

# search source
oaCodes5 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = "Condition",
  searchSource = TRUE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE,
  db = db,
  vocabularyDatabaseSchema = vocabularyDatabaseSchema
)
saveRDS(
  oaCodes5,
  here("vignettes", "optionsData04.RData")
)



# fuzzy search
oaCodes6 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = "Condition",
  searchSynonyms = FALSE,
  fuzzyMatch = TRUE,
  maxDistanceCost = 0.1,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE,
  db = db,
  vocabularyDatabaseSchema = vocabularyDatabaseSchema
)
saveRDS(
  oaCodes6,
  here("vignettes", "optionsData05.RData")
)

# fuzzy search 0.2
oaCodes7 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = "Condition",
  searchSynonyms = FALSE,
  fuzzyMatch = TRUE,
  maxDistanceCost = 0.2,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE,
  db = db,
  vocabularyDatabaseSchema = vocabularyDatabaseSchema
)
saveRDS(
  oaCodes7,
  here("vignettes", "optionsData06.RData")
)

# include ancestor
oaCodes8 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = "Condition",
  searchSynonyms = FALSE,
  fuzzyMatch = FALSE,
  maxDistanceCost = 0.2,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = TRUE,
  db = db,
  vocabularyDatabaseSchema = vocabularyDatabaseSchema
)
saveRDS(
  oaCodes8,
  here("vignettes", "optionsData07.RData")
)

# colonoscopy vignette ------
codesFromDescendants<-tbl(db,
  sql(paste0("SELECT * FROM ",
     vocabularyDatabaseSchema,
     ".concept_ancestor"))) %>%
  filter(ancestor_concept_id %in% c("4249893", "937652", "40480729")) %>%
  select("descendant_concept_id") %>%
  rename("concept_id"="descendant_concept_id") %>%
  inner_join(tbl(db, sql(paste0("SELECT * FROM ",
     vocabularyDatabaseSchema,
     ".concept"))))%>%
  select("concept_id", "concept_name",
         "domain_id", "vocabulary_id") %>%
  collect()

saveRDS(
  codesFromDescendants,
  here("vignettes", "procData01.RData")
)



colonoscopyCodes2<-getCandidateCodes(keywords="colonoscopy",
                    domains=c("Procedure", "Measurement"),
                    searchSynonyms = FALSE,
                    fuzzyMatch = FALSE,
                    exclude = NULL,
                    includeDescendants = FALSE,
                    includeAncestor = FALSE,
                    verbose = TRUE ,
                    db=db,
                    vocabularyDatabaseSchema =  vocabularyDatabaseSchema)
saveRDS(
  colonoscopyCodes2,
  here("vignettes", "procData02.RData")
)



# medication vignette ------
codesFromDescendants<-tbl(db,
  sql(paste0("SELECT * FROM ",
     vocabularyDatabaseSchema,
     ".concept_ancestor"))) %>%
  filter(ancestor_concept_id %in% c("1503297")) %>%
  select("descendant_concept_id") %>%
  rename("concept_id"="descendant_concept_id") %>%
  inner_join(tbl(db, sql(paste0("SELECT * FROM ",
     vocabularyDatabaseSchema,
     ".concept"))))%>%
  select("concept_id", "concept_name",
         "domain_id", "vocabulary_id") %>%
  collect()

saveRDS(
  codesFromDescendants,
  here("vignettes", "medData01.RData")
)



metforminCodes2<-getCandidateCodes(keywords="metformin",
                    domains=c("Drug"),
                    standardConcept=c("Standard", "Classification"),
                    searchSynonyms = FALSE,
                    fuzzyMatch = FALSE,
                    exclude = NULL,
                    includeDescendants = TRUE,
                    includeAncestor = FALSE,
                    verbose = TRUE ,
                    db=db,
                    vocabularyDatabaseSchema =  vocabularyDatabaseSchema)
saveRDS(
  metforminCodes2,
  here("vignettes", "metforminCodes2.RData")
)



