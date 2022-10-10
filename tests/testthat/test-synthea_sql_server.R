test_that("test with synthea on sql server", {

  skip_if(Sys.getenv("darwinDbDatabaseServer") == "")

  db <- DBI::dbConnect(odbc::odbc(),
                       Driver   = "ODBC Driver 11 for SQL Server",
                       Server   = Sys.getenv("darwinDbDatabaseServer"),
                       Database = Sys.getenv("darwinDbDatabase"),
                       UID      = Sys.getenv("darwinDbUser"),
                       PWD      = Sys.getenv("darwinDbPassword"),
                       Port     = Sys.getenv("darwinDbDatabasePort"))

  cdm <- CDMConnector::cdm_from_con(con = db,
                      cdm_schema = "cdm_synthea_100k",
                      cdm_tables = tidyselect::all_of(c("concept",
                                                        "concept_relationship",
                                                        "concept_ancestor",
                                                        "concept_synonym",
                                                        "vocabulary")))
 vocab_version <- getVocabVersion(cdm=cdm)
 expect_true(length(vocab_version)==1)
 expect_true(is.character(vocab_version))

 # search in database
 asthma_codes<-getCandidateCodes(cdm=cdm,
                                  keywords="asthma",
                                  domains = "Condition")
 expect_true(all(c(
   "concept_id", "concept_name",
   "domain_id", "vocabulary_id"
 ) %in%
   names(asthma_codes)))
 expect_true(nrow(asthma_codes)>0)

 asthmaIcdMappings<-getMappings(cdm=cdm,
                                 candidateCodelist=asthma_codes,
                                 nonStandardVocabularies="ICD10CM")
 expect_true(all(c(
   "standard_concept_id",
   "standard_concept_name",
   "standard_vocabulary_id",
   "non_standard_concept_id",
   "non_standard_concept_name",
   "non_standard_concept_code",
   "non_standard_vocabulary_id"
 ) %in%
   names(asthmaIcdMappings)))
 expect_true(nrow(asthmaIcdMappings)>0)

 dbDisconnect(db)

})

