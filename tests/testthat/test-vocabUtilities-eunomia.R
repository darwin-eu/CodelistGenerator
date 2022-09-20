test_that("tests with mock db", {

  # eunomia
  db <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(db, cdm_schema = "main",
                                    cdm_tables = tidyselect::all_of(c("concept",
                                                                      "concept_relationship",
                                                                      "concept_ancestor",
                                                                      "concept_synonym",
                                                                      "vocabulary")))


  version <- getVocabVersion(cdm=cdm)
  expect_true(length(version)==1)
  expect_true(is.character(version))

  vocabs <- getVocabularies(cdm=cdm)
  expect_true(length(vocabs)>=1)
  expect_true(is.character(vocabs))

  domains<-getDomains(cdm=cdm)
  expect_true(all(c("Condition", "Observation") %in% domains))
  expect_true(is.character(domains))

  concept_classes <- getconceptClassId(cdm=cdm)
  expect_true(is.character(concept_classes))

  concept_classes <- getconceptClassId(cdm=cdm,
                    domain = "Condition")
  expect_true(is.character(concept_classes))

  DBI::dbDisconnect(db)


})
