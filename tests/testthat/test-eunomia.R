test_that("vocabUtilities", {

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

  descendants <- getDescendants(cdm=cdm,
                                concept_id = 19054876)
  expect_true(nrow(descendants)>0)

  DBI::dbDisconnect(db)


})

test_that("getCandidateCodes", {

  # eunomia
  db <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(db, cdm_schema = "main",
                                    cdm_tables = tidyselect::all_of(c("concept",
                                                                      "concept_relationship",
                                                                      "concept_ancestor",
                                                                      "concept_synonym",
                                                                      "vocabulary")))

  # test keywords search - exact - all options
  codes1 <- getCandidateCodes(
    cdm=cdm,
    keywords = "Fracture",
    exclude = "Laceration",
    domains = "Condition",
    searchInSynonyms = TRUE,
    searchViaSynonyms = TRUE,
    includeDescendants = TRUE,
    includeAncestor = TRUE
  )
  # test keywords search - exact - all options
  codes2 <- getCandidateCodes(
    cdm=cdm,
    keywords = "Fracture",
    exclude = "Laceration",
    fuzzyMatch = TRUE,
    domains = "Condition",
    searchInSynonyms = TRUE,
    searchViaSynonyms = TRUE,
    includeDescendants = TRUE,
    includeAncestor = TRUE
  )

  # variable names
  expect_true(all(c(
    "concept_id", "concept_name",
    "domain_id", "vocabulary_id"
  ) %in%
    names(codes1)))

  expect_true(all(c(
    "concept_id", "concept_name",
    "domain_id", "vocabulary_id"
  ) %in%
    names(codes2)))

  DBI::dbDisconnect(db)

})



