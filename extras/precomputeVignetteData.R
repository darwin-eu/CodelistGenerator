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

arrowDirectory <- Sys.getenv("VocabArrowPath")



# intro vignette ----
vocabVersion <- getVocabVersion(arrowDirectory = arrowDirectory)

saveRDS(
  vocabVersion,
  here("vignettes", "introVocab.RData")
)


codesFromDescendants <- arrow::read_parquet(paste0(arrowDirectory,
                                                   "/concept_ancestor.parquet"),
                                            as_data_frame = FALSE)%>%
  filter(.data$ancestor_concept_id == 4182210) %>%
  select("descendant_concept_id") %>%
  rename("concept_id" = "descendant_concept_id") %>%
  select("concept_id")   %>%
  left_join(arrow::read_parquet(paste0(arrowDirectory,
                                       "/concept.parquet"),
                                as_data_frame = FALSE),
            by="concept_id")  %>%
  select("concept_id", "concept_name", "domain_id", "vocabulary_id") %>%
  collect()

saveRDS(
  codesFromDescendants,
  here("vignettes", "introData01.RData")
)

dementiaCodes1 <- getCandidateCodes(
  keywords = "dementia",
  domains = "Condition",
  searchViaSynonyms = FALSE,
  fuzzyMatch = FALSE,
  exclude = NULL,
  includeDescendants = TRUE,
  includeAncestor = FALSE,
  arrowDirectory = Sys.getenv("VocabArrowPath"),
  verbose=TRUE
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

icdMappings <- getMappings(
  candidateCodelist = dementiaCodes1,
  nonStandardVocabularies = "ICD10CM",
  arrowDirectory = Sys.getenv("VocabArrowPath")
)
saveRDS(
  icdMappings,
  here("vignettes", "introData04.RData")
)

readMappings <- getMappings(
  candidateCodelist = dementiaCodes1,
  nonStandardVocabularies = "Read",
  arrowDirectory = Sys.getenv("VocabArrowPath")
)
saveRDS(
  readMappings,
  here("vignettes", "introData05.RData")
)


# options vignette ------
oaCodes1 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = "Condition",
  searchViaSynonyms = FALSE,
  fuzzyMatch = FALSE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE,
  arrowDirectory = Sys.getenv("VocabArrowPath")
)
saveRDS(
  oaCodes1,
  here("vignettes", "optionsData01.RData")
)

# include desc
oaCodes2 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = "Condition",
  searchViaSynonyms = FALSE,
  fuzzyMatch = FALSE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = TRUE,
  includeAncestor = FALSE,
  arrowDirectory = Sys.getenv("VocabArrowPath")
)
saveRDS(
  oaCodes2,
  here("vignettes", "optionsData02.RData")
)

# include obs
oaCodes3 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = c("Condition", "Observation"),
  searchViaSynonyms = FALSE,
  fuzzyMatch = FALSE,
  maxDistanceCost = 0.1,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE,
  arrowDirectory = Sys.getenv("VocabArrowPath")
)
saveRDS(
  oaCodes3,
  here("vignettes", "optionsData03.RData")
)

# search syn
oaCodes4 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = "Condition",
  searchInSynonyms = TRUE,
  searchViaSynonyms = TRUE,
  fuzzyMatch = FALSE,
  maxDistanceCost = 0.1,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE,
  arrowDirectory = Sys.getenv("VocabArrowPath")
)
saveRDS(
  oaCodes4,
  here("vignettes", "optionsData04.RData")
)

# search source
oaCodes5 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = "Condition",
  searchNonStandard = TRUE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE,
  arrowDirectory = Sys.getenv("VocabArrowPath")
)
saveRDS(
  oaCodes5,
  here("vignettes", "optionsData04.RData")
)



# fuzzy search
oaCodes6 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = "Condition",
  searchViaSynonyms = FALSE,
  fuzzyMatch = TRUE,
  maxDistanceCost = 0.1,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE,
  arrowDirectory = Sys.getenv("VocabArrowPath")
)
saveRDS(
  oaCodes6,
  here("vignettes", "optionsData05.RData")
)

# fuzzy search 0.2
oaCodes7 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = "Condition",
  searchViaSynonyms = FALSE,
  fuzzyMatch = TRUE,
  maxDistanceCost = 0.2,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE,
  arrowDirectory = Sys.getenv("VocabArrowPath")
)
saveRDS(
  oaCodes7,
  here("vignettes", "optionsData06.RData")
)

# include ancestor
oaCodes8 <- getCandidateCodes(
  keywords = "osteoarthritis",
  domains = "Condition",
  searchViaSynonyms = FALSE,
  fuzzyMatch = FALSE,
  maxDistanceCost = 0.2,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = TRUE,
  arrowDirectory = Sys.getenv("VocabArrowPath")
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
                    searchViaSynonyms = FALSE,
                    fuzzyMatch = FALSE,
                    exclude = NULL,
                    includeDescendants = FALSE,
                    includeAncestor = FALSE,
                    verbose = TRUE ,
                    arrowDirectory = Sys.getenv("VocabArrowPath"))
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
                    conceptClassId="Ingredient",
                    searchViaSynonyms = FALSE,
                    fuzzyMatch = FALSE,
                    exclude = NULL,
                    includeDescendants = TRUE,
                    includeAncestor = FALSE,
                    verbose = TRUE ,
                    arrowDirectory = Sys.getenv("VocabArrowPath"))
saveRDS(
  metforminCodes2,
  here("vignettes", "metforminCodes2.RData")
)


getCandidateCodes(keywords="metformin",
                    domains=c("Drug"),
                    standardConcept=c("Standard", "Classification"),
                    conceptClassId="Ingredient",
                    searchViaSynonyms = FALSE,
                    fuzzyMatch = FALSE,
                    exclude = NULL,
                    includeDescendants = TRUE,
                    includeAncestor = FALSE,
                    verbose = TRUE ,
                  arrowDirectory = Sys.getenv("VocabArrowPath"))
getCandidateCodes(keywords="metformin",
                    domains=c("Drug"),
                    standardConcept=c("Standard", "Classification"),
                    conceptClassId="Prescription Drug",
                    searchViaSynonyms = FALSE,
                    fuzzyMatch = FALSE,
                    exclude = NULL,
                    includeDescendants = TRUE,
                    includeAncestor = FALSE,
                    verbose = TRUE ,
                  arrowDirectory = Sys.getenv("VocabArrowPath"))

