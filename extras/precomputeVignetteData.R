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
                                  cdm_tables = tidyselect::all_of(c("concept",
                                                                    "concept_relationship",
                                                                    "concept_ancestor",
                                                                    "concept_synonym",
                                                                    "drug_strength",
                                                                    "vocabulary")))
# vocab to arrow
# save in temp folder for this example
dOut<-here(tempdir(), "db_vocab")
dir.create(dOut)
CDMConnector::stow(cdm, dOut)

# new cdm reference using arrow
cdm_arrow <- CDMConnector::cdm_from_files(path = dOut,
                                          as_data_frame = FALSE)

rm(cdm)

# intro vignette ----
vocabVersion <- getVocabVersion(cdm = cdm_arrow)

save(
  vocabVersion,
  file = here("vignettes", "introVocab.RData")
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
  #searchViaSynonyms = FALSE,
  #fuzzyMatch = FALSE,
  exclude = NULL,
  includeDescendants = TRUE,
  includeAncestor = FALSE,
  #verbose=TRUE
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
  #searchViaSynonyms = FALSE,
  #fuzzyMatch = FALSE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  includeDescendants = FALSE,
  includeAncestor = FALSE,
  #verbose = TRUE
)
saveRDS(
  oaCodes1,
  here("vignettes", "optionsData01.RData")
)

# include desc
oaCodes2 <- getCandidateCodes(cdm = cdm_arrow,
  keywords = "osteoarthritis",
  domains = "Condition",
  #searchViaSynonyms = FALSE,
  #fuzzyMatch = FALSE,
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
  #searchViaSynonyms = FALSE,
  #fuzzyMatch = FALSE,
  #maxDistanceCost = 0.1,
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
  #searchViaSynonyms = TRUE,
  #fuzzyMatch = FALSE,
  #maxDistanceCost = 0.1,
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
  #searchViaSynonyms = FALSE,
  #fuzzyMatch = TRUE,
  #maxDistanceCost = 0.1,
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
  #searchViaSynonyms = FALSE,
  #fuzzyMatch = TRUE,
  #maxDistanceCost = 0.2,
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
  #searchViaSynonyms = FALSE,
  #fuzzyMatch = FALSE,
  #maxDistanceCost = 0.2,
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

# medication vignette ------
ac_codes_1 <- getCandidateCodes(cdm = cdm_arrow,
                                     keywords="acetaminophen",
                                     domains="drug",
                                     standardConcept="standard",
                                     includeDescendants = TRUE)
saveRDS(
  ac_codes_1,
  here("vignettes", "medData01.RData")
)

ac_codes_2a <- getCandidateCodes(cdm = cdm_arrow,
                                 keywords= c("acetaminophen injection",
                                             "acetaminophen intravenous"),
                                 domains="drug",
                                 standardConcept="standard",
                                 includeDescendants = TRUE)
saveRDS(
  ac_codes_2a,
  here("vignettes", "medData02a.RData")
)

ac_codes_2b <- getCandidateCodes(cdm = cdm_arrow,
                                keywords="acetaminophen",
                                domains="drug",
                                #doseForm = c("injection", "intravenous"),
                                standardConcept="standard",
                                includeDescendants = TRUE)
saveRDS(
  ac_codes_2b,
  here("vignettes", "medData02b.RData")
)

ac_dose_forms <- CodelistGenerator::getDoseForm(cdm = cdm_arrow)
saveRDS(
  ac_dose_forms,
  here("vignettes", "medDataDoseForms.RData"))

c <- compareCodelists(ac_codes_2, ac_codes_2a)
ac_codes_3 <- getCandidateCodes(cdm = cdm_arrow,
                                keywords="acetaminophen",
                                domains="drug",
                                #conceptClassId = c("Quant Clinical Drug"),
                                #doseForm = c("injection", "intravenous"),
                                standardConcept="standard",
                                includeDescendants = TRUE)
saveRDS(
  ac_codes_3,
  here("vignettes", "medData03.RData")
)

ac_concept_class <- CodelistGenerator::getConceptClassId(cdm = cdm_arrow,
                                                         domain = "drug")
saveRDS(
  ac_concept_class,
  here("vignettes", "medDataConceptClass.RData"))
