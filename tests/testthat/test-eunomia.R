test_that("vocabUtilities", {
  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
  skip_if_not(rlang::is_installed("duckdb", version = "0.6"))
  skip_if_not(CDMConnector::eunomia_is_available())

  db <- DBI::dbConnect(duckdb::duckdb(),
                       dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(db, cdm_schema = "main",
                                    cdm_tables = tidyselect::all_of(c("concept",
                                                                      "concept_relationship",
                                                                      "concept_ancestor",
                                                                      "concept_synonym",
                                                                      "vocabulary",
                                                                      "drug_strength")))


  version <- getVocabVersion(cdm=cdm)
  expect_true(length(version)==1)
  expect_true(is.character(version))

  vocabs <- getVocabularies(cdm=cdm)
  expect_true(length(vocabs)>=1)
  expect_true(is.character(vocabs))

  domains<-getDomains(cdm=cdm)
  expect_true(all(c("Condition", "Observation") %in% domains))
  expect_true(is.character(domains))

  concept_classes <- getConceptClassId(cdm=cdm)
  expect_true(is.character(concept_classes))

  concept_classes <- getConceptClassId(cdm=cdm,
                                       domain = "Condition")
  expect_true(is.character(concept_classes))

  descendants <- getDescendants(cdm=cdm,
                                concept_id = 19054876)
  expect_true(nrow(descendants)>0)

  DBI::dbDisconnect(db)


})

test_that("getCandidateCodes", {
  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
  skip_if_not(rlang::is_installed("duckdb", version = "0.6"))
  skip_if_not(CDMConnector::eunomia_is_available())

  db <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(db, cdm_schema = "main",
                                    cdm_tables = tidyselect::all_of(c("concept",
                                                                      "concept_relationship",
                                                                      "concept_ancestor",
                                                                      "concept_synonym",
                                                                      "vocabulary",
                                                                      "drug_strength")))

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
  # test search in drug
  codes3 <- getCandidateCodes(
    cdm=cdm,
    keywords = "Diclofenac",
    fuzzyMatch = TRUE,
    domains = "Drug",
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

  expect_true(all(c(
    "concept_id", "concept_name",
    "domain_id", "vocabulary_id","found_from", "ingredient_concept_id", "amount_value", "amount_unit_concept_id",
    "numerator_value", "numerator_unit_concept_id", "denominator_value", "denominator_unit_concept_id", "box_size"
  ) %in%
    names(codes3)))

  DBI::dbDisconnect(db)

})



