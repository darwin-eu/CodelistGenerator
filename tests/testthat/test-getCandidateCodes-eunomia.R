test_that("tests with eunomia", {


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



