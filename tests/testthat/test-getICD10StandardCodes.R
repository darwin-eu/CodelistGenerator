test_that("db without icd10 codes loaded", {
  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backend = backends[i])
    codes <- getICD10StandardCodes(cdm = cdm,
                                 level = c(
                                   "ICD10 Chapter",
                                   "ICD10 SubChapter"
                                 ))
  expect_true(length(codes) == 2)
  expect_true("arthropathies" %in% names(codes))
  expect_true("diseases_of_the_musculoskeletal_system_and_connective_tissue" %in%
                names(codes))
  # we should pick up mapping and descendants
  expect_true(all(c(3,4,5) %in% codes[[1]]))
  expect_true(all(c(3,4,5) %in% codes[[2]]))

  # without descendants
  codes <- getICD10StandardCodes(cdm = cdm,
                                 level = c(
                                   "ICD10 Chapter",
                                   "ICD10 SubChapter"
                                 ), includeDescendants = FALSE)
  expect_true(length(codes) == 2)
  # we should pick up mapping and descendants
  expect_true(all(c(3) %in% codes[[1]]))
  expect_true(all(c(3) %in% codes[[2]]))

  # with only sub-chapter
  codes2 <- getICD10StandardCodes(cdm = cdm,
                                 level = "ICD10 SubChapter")
  expect_true(length(codes2) == 1)

  # specific name
  codes3 <- getICD10StandardCodes(cdm = cdm,
                                  level = "ICD10 SubChapter",
                                  name = "Arthropathies")
  expect_true(length(codes3) == 1)

  codes4 <- getICD10StandardCodes(cdm = cdm,
                                  level = "ICD10 SubChapter",
                                  name = "XYZ")
  expect_true(length(codes4) == 0)

  # with details
  codes5 <- getICD10StandardCodes(cdm = cdm,
                                  level = "ICD10 SubChapter",
                                  withConceptDetails = TRUE)
  expect_true(!is.null(codes5[[1]]$concept_name))

  if (backends[[i]] == "database") {
    CDMConnector::cdm_disconnect(cdm)
  }

  }
})

test_that("db without icd10 codes loaded", {
  cdm <- mockVocabRef()
  cdm$concept <- cdm$concept %>%
    dplyr::filter(vocabulary_id != "ICD10")
  expect_message(codes <- getICD10StandardCodes(cdm = cdm))
  expect_true(length(codes) == 0)
  CDMConnector::cdm_disconnect(cdm)
})

test_that("expected errors", {
  cdm <- mockVocabRef()
  expect_error(getICD10StandardCodes(cdm = "not a cdm"))
  expect_error(getICD10StandardCodes(cdm = cdm, level = c(
    "Not an ICD10 Chapter"
  )))
  CDMConnector::cdm_disconnect(cdm)
})
