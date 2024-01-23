## precompute data for vignettes -----
# to avoid fails in continuous integration of vignettes
# (because of the use of external data)
# will create the data for vignettes here
library(readr)
library(DBI)
library(here)
library(dplyr)
library(dbplyr)
library(stringr)
library(DT)
library(kableExtra)
devtools::load_all()

# example with postgres database connection details
db <- dbConnect(RPostgres::Postgres(),
                dbname = "cdm_gold_202301",
                port = Sys.getenv("DB_PORT") ,
                host = "163.1.65.51",
                user = Sys.getenv("DB_USER"),
                password =  Sys.getenv("DB_PASSWORD"))
# name of vocabulary schema
vocabularyDatabaseSchema <- Sys.getenv("DB_VOCAB_SCHEMA")

# create cdm reference
cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = vocabularyDatabaseSchema,
                                  write_schema = "results")

# intro vignette ----
vocabVersion <- getVocabVersion(cdm = cdm)

codesFromDescendants <- cdm$concept_ancestor %>%
  filter(.data$ancestor_concept_id == 4182210) %>%
  select("descendant_concept_id") %>%
  rename("concept_id" = "descendant_concept_id") %>%
  select("concept_id")   %>%
  left_join(cdm$concept,
            by="concept_id")  %>%
  select("concept_id", "concept_name", "domain_id", "vocabulary_id") %>%
  collect()


dementiaCodes1 <- getCandidateCodes(cdm = cdm,
  keywords = "dementia",
  domains = "Condition",
  exclude = NULL,
  includeDescendants = TRUE,
  includeAncestor = FALSE,
)


codeComparison <- compareCodelists(codesFromDescendants,
                  dementiaCodes1)


icdMappings <- getMappings(cdm = cdm,
  candidateCodelist = dementiaCodes1,
  nonStandardVocabularies = "ICD10CM"
)


readMappings <- getMappings(cdm = cdm,
  candidateCodelist = dementiaCodes1,
  nonStandardVocabularies = "Read"
)


# options vignette ------
oaCodes1 <- getCandidateCodes(cdm = cdm,
  keywords = "osteoarthritis",
  domains = "Condition",
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE,
  #verbose = TRUE
)

# include desc
oaCodes2 <- getCandidateCodes(cdm = cdm,
  keywords = "osteoarthritis",
  domains = "Condition",
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = TRUE,
  includeAncestor = FALSE
)

# include obs
oaCodes3 <- getCandidateCodes(cdm = cdm,
  keywords = "osteoarthritis",
  domains = c("Condition", "Observation"),
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE
)

# search syn
oaCodes4 <- getCandidateCodes(cdm = cdm,
  keywords = "osteoarthritis",
  domains = "Condition",
  searchInSynonyms = TRUE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE
)


# search source
oaCodes5 <- getCandidateCodes(cdm = cdm,
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


# fuzzy search
oaCodes6 <- getCandidateCodes(cdm = cdm,
  keywords = "osteoarthritis",
  domains = "Condition",
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE
)


# fuzzy search 0.2
oaCodes7 <- getCandidateCodes(cdm = cdm,
  keywords = "osteoarthritis",
  domains = "Condition",
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE
)


# include ancestor
oaCodes8 <- getCandidateCodes(cdm = cdm,
  keywords = "osteoarthritis",
  domains = "Condition",
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = TRUE
)

# medication vignette ------
ac_codes_1 <- getCandidateCodes(cdm = cdm,
                                     keywords="acetaminophen",
                                     domains="drug",
                                     standardConcept="standard",
                                     includeDescendants = TRUE)

ac_codes_2a <- getCandidateCodes(cdm = cdm,
                                 keywords= c("acetaminophen injection",
                                             "acetaminophen intravenous"),
                                 domains="drug",
                                 standardConcept="standard",
                                 includeDescendants = TRUE)


ac_codes_2b <- getCandidateCodes(cdm = cdm,
                                keywords="acetaminophen",
                                domains="drug",
                                #doseForm = c("injection", "intravenous"),
                                standardConcept="standard",
                                includeDescendants = TRUE)


ac_dose_forms <- CodelistGenerator::getDoseForm(cdm = cdm)


c <- compareCodelists(ac_codes_2a, ac_codes_2b)
ac_codes_3 <- getCandidateCodes(cdm = cdm,
                                keywords="acetaminophen",
                                domains="drug",
                                #conceptClassId = c("Quant Clinical Drug"),
                                #doseForm = c("injection", "intravenous"),
                                standardConcept="standard",
                                includeDescendants = TRUE)

ac_concept_class <- CodelistGenerator::getConceptClassId(cdm = cdm,
                                                         domain = "drug")


# save -----
save(
  vocabVersion,
  file = here("inst", "introVocab.RData")
)
saveRDS(
  codesFromDescendants,
  here("inst", "introData01.RData")
)
saveRDS(
  dementiaCodes1,
  here("inst", "introData02.RData")
)
saveRDS(
  codeComparison,
  here("inst", "introData03.RData")
)
saveRDS(
  icdMappings,
  here("inst", "introData04.RData")
)
saveRDS(
  readMappings,
  here("inst", "introData05.RData")
)
saveRDS(
  oaCodes1,
  here("inst", "optionsData01.RData")
)
saveRDS(
  oaCodes2,
  here("inst", "optionsData02.RData")
)
saveRDS(
  oaCodes3,
  here("inst", "optionsData03.RData")
)
saveRDS(
  oaCodes4,
  here("inst", "optionsData04.RData")
)
saveRDS(
  oaCodes5,
  here("inst", "optionsData04.RData")
)
saveRDS(
  oaCodes6,
  here("inst", "optionsData05.RData")
)
saveRDS(
  oaCodes7,
  here("inst", "optionsData06.RData")
)
saveRDS(
  oaCodes8,
  here("inst", "optionsData07.RData")
)
saveRDS(
  ac_codes_1,
  here("inst", "medData01.RData")
)
saveRDS(
  ac_codes_2a,
  here("inst", "medData02a.RData")
)
saveRDS(
  ac_codes_2b,
  here("inst", "medData02b.RData")
)
saveRDS(
  ac_dose_forms,
  here("inst", "medDataDoseForms.RData")
)
saveRDS(
  ac_codes_3,
  here("inst", "medData03.RData")
)
saveRDS(
  ac_concept_class,
  here("inst", "medDataConceptClass.RData")
)
