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

# example with postgres database connection details
db <- DBI::dbConnect(RPostgres::Postgres(),
                     dbname = Sys.getenv("DB_SERVER_cdm_aurum_202106_dbi"),
                     port = Sys.getenv("DB_PORT"),
                     host = Sys.getenv("DB_HOST"),
                     user = Sys.getenv("DB_USER"),
                     password = Sys.getenv("DB_PASSWORD")
)
# name of vocabulary schema
vocabularyDatabaseSchema <- Sys.getenv("DB_VOCAB_SCHEMA")

# create cdm reference
cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = vocabularyDatabaseSchema,
                                  cdm_tables = tidyselect::all_of(c("concept",
                                                                    "concept_relationship",
                                                                    "concept_ancestor",
                                                                    "concept_synonym",
                                                                    "vocabulary")))
# vocab to arrow
# save in temp folder for this example
dOut<-here(tempdir(), "db_vocab")
dir.create(dOut)
CDMConnector::stow(cdm, dOut)

# new cdm reference using arrow
cdm_arrow <- CDMConnector::cdm_from_files(path = dOut,
                                          cdm_tables = tidyselect::all_of(c("concept",
                                                                            "concept_relationship",
                                                                            "concept_ancestor",
                                                                            "concept_synonym",
                                                                            "vocabulary")),
                                          as_data_frame = FALSE)

rm(cdm)

# intro vignette ----
vocabVersion <- getVocabVersion(cdm = cdm_arrow)

saveRDS(
  vocabVersion,
  here("vignettes", "introVocab.RData")
)


codesFromDescendants <- cdm_arrow$concept_ancestor %>%
  filter(.data$ancestor_concept_id == 4182210) %>%
  select("descendant_concept_id") %>%
  rename("concept_id" = "descendant_concept_id") %>%
  select("concept_id")   %>%
  left_join(cdm_arrow$concept,
            by="concept_id")  %>%
  select("concept_id", "concept_name", "domain_id", "vocabulary_id") %>%
  collect()

saveRDS(
  codesFromDescendants,
  here("vignettes", "introData01.RData")
)

dementiaCodes1 <- getCandidateCodes(cdm = cdm_arrow,
  keywords = "dementia",
  domains = "Condition",
  searchViaSynonyms = FALSE,
  fuzzyMatch = FALSE,
  exclude = NULL,
  includeDescendants = TRUE,
  includeAncestor = FALSE,
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

icdMappings <- getMappings(cdm = cdm_arrow,
  candidateCodelist = dementiaCodes1,
  nonStandardVocabularies = "ICD10CM"
)
saveRDS(
  icdMappings,
  here("vignettes", "introData04.RData")
)

readMappings <- getMappings(cdm = cdm_arrow,
  candidateCodelist = dementiaCodes1,
  nonStandardVocabularies = "Read"
)
saveRDS(
  readMappings,
  here("vignettes", "introData05.RData")
)


# options vignette ------
oaCodes1 <- getCandidateCodes(cdm = cdm_arrow,
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
  verbose = TRUE
)
saveRDS(
  oaCodes1,
  here("vignettes", "optionsData01.RData")
)

# include desc
oaCodes2 <- getCandidateCodes(cdm = cdm_arrow,
  keywords = "osteoarthritis",
  domains = "Condition",
  searchViaSynonyms = FALSE,
  fuzzyMatch = FALSE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = TRUE,
  includeAncestor = FALSE
)
saveRDS(
  oaCodes2,
  here("vignettes", "optionsData02.RData")
)

# include obs
oaCodes3 <- getCandidateCodes(cdm = cdm_arrow,
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
  includeAncestor = FALSE
)
saveRDS(
  oaCodes3,
  here("vignettes", "optionsData03.RData")
)

# search syn
oaCodes4 <- getCandidateCodes(cdm = cdm_arrow,
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
  includeAncestor = FALSE
)
saveRDS(
  oaCodes4,
  here("vignettes", "optionsData04.RData")
)

# search source
oaCodes5 <- getCandidateCodes(cdm = cdm_arrow,
  keywords = "osteoarthritis",
  domains = "Condition",
  searchNonStandard = TRUE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE
)
saveRDS(
  oaCodes5,
  here("vignettes", "optionsData04.RData")
)



# fuzzy search
oaCodes6 <- getCandidateCodes(cdm = cdm_arrow,
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
  includeAncestor = FALSE
)
saveRDS(
  oaCodes6,
  here("vignettes", "optionsData05.RData")
)

# fuzzy search 0.2
oaCodes7 <- getCandidateCodes(cdm = cdm_arrow,
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
  includeAncestor = FALSE
)
saveRDS(
  oaCodes7,
  here("vignettes", "optionsData06.RData")
)

# include ancestor
oaCodes8 <- getCandidateCodes(cdm = cdm_arrow,
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
  includeAncestor = TRUE
)
saveRDS(
  oaCodes8,
  here("vignettes", "optionsData07.RData")
)

# colonoscopy vignette ------
codesFromDescendants<-cdm_arrow$concept_ancestor %>%
  filter(ancestor_concept_id %in% c("4249893", "937652", "40480729")) %>%
  select("descendant_concept_id") %>%
  rename("concept_id"="descendant_concept_id") %>%
  inner_join(cdm_arrow$concept) %>%
  select("concept_id", "concept_name",
         "domain_id", "vocabulary_id") %>%
  collect()

saveRDS(
  codesFromDescendants,
  here("vignettes", "procData01.RData")
)



colonoscopyCodes2<-getCandidateCodes(cdm = cdm_arrow,
                                     keywords="colonoscopy",
                    domains=c("Procedure", "Measurement"),
                    searchViaSynonyms = FALSE,
                    fuzzyMatch = FALSE,
                    exclude = NULL,
                    includeDescendants = FALSE,
                    includeAncestor = FALSE,
                    verbose = TRUE )
saveRDS(
  colonoscopyCodes2,
  here("vignettes", "procData02.RData")
)



# medication vignette ------
codesFromDescendants<-cdm_arrow$concept_ancestor %>%
  filter(ancestor_concept_id %in% c("1503297")) %>%
  select("descendant_concept_id") %>%
  rename("concept_id"="descendant_concept_id") %>%
  inner_join(cdm_arrow$concept )%>%
  select("concept_id", "concept_name",
         "domain_id", "vocabulary_id") %>%
  collect()

saveRDS(
  codesFromDescendants,
  here("vignettes", "medData01.RData")
)



metforminCodes2<-getCandidateCodes(cdm = cdm_arrow,
                                   keywords="metformin",
                    domains=c("Drug"),
                    standardConcept=c("Standard", "Classification"),
                    conceptClassId="Ingredient",
                    searchViaSynonyms = FALSE,
                    fuzzyMatch = FALSE,
                    exclude = NULL,
                    includeDescendants = TRUE,
                    includeAncestor = FALSE,
                    verbose = TRUE )
saveRDS(
  metforminCodes2,
  here("vignettes", "metforminCodes2.RData")
)


getCandidateCodes(cdm = cdm_arrow,
  keywords="metformin",
                    domains=c("Drug"),
                    standardConcept=c("Standard", "Classification"),
                    conceptClassId="Ingredient",
                    searchViaSynonyms = FALSE,
                    fuzzyMatch = FALSE,
                    exclude = NULL,
                    includeDescendants = TRUE,
                    includeAncestor = FALSE,
                    verbose = TRUE )
getCandidateCodes(cdm = cdm_arrow,
                  keywords="metformin",
                    domains=c("Drug"),
                    standardConcept=c("Standard", "Classification"),
                    conceptClassId="Prescription Drug",
                    searchViaSynonyms = FALSE,
                    fuzzyMatch = FALSE,
                    exclude = NULL,
                    includeDescendants = TRUE,
                    includeAncestor = FALSE,
                    verbose = TRUE )

