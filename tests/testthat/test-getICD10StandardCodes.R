test_that("db without icd10 codes loaded", {

  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm   <- mockVocabRef(backend = backends[i])
    codes <- getICD10StandardCodes(cdm = cdm,
                                   nameStyle = "{concept_name}",
                                   level = c(
                                     "ICD10 Chapter",
                                     "ICD10 SubChapter"
                                   ))
    expect_true(length(codes) == 2)
    expect_true("arthropathies" %in% names(codes))
    expect_true("diseases_of_the_musculoskeletal_system_and_connective_tissue" %in%
                  names(codes))

    # check nameStyle
    codes11 <- getICD10StandardCodes(cdm = cdm,
                                     nameStyle = "{concept_code}_{concept_name}")
    expect_identical(codes11[[1]], codes[[1]])
    expect_identical(names(codes11),
                     c("1234_arthropathies",
                       "1234_diseases_of_the_musculoskeletal_system_and_connective_tissue"))
    codes12 <- getICD10StandardCodes(cdm = cdm,
                                     nameStyle = "{concept_id}_{concept_name}")
    expect_identical(codes12[[1]], codes[[1]])
    expect_identical(names(codes12),
                     c("18_arthropathies",
                       "18_diseases_of_the_musculoskeletal_system_and_connective_tissue"))

    # we should pick up mapping and descendants
    expect_true(all(c(3,4,5) %in% codes[[1]]))
    expect_true(all(c(3,4,5) %in% codes[[2]]))

    # without descendants
    codes <- getICD10StandardCodes(cdm = cdm,
                                   level = c("ICD10 Chapter","ICD10 SubChapter"),
                                   nameStyle = "{concept_name}",
                                   includeDescendants = FALSE)
    expect_true(length(codes) == 2)

    # we should pick up mapping and descendants
    expect_true(all(c(3) %in% codes[[1]]))
    expect_true(all(c(3) %in% codes[[2]]))

    # with only sub-chapter
    codes2 <- getICD10StandardCodes(cdm = cdm,
                                    nameStyle = "{concept_name}",
                                    level = "ICD10 SubChapter")
    expect_true(length(codes2) == 1)

    # specific name
    codes3 <- getICD10StandardCodes(cdm = cdm,
                                    level = "ICD10 SubChapter",
                                    nameStyle = "{concept_name}",
                                    name = "Arthropathies")
    expect_true(length(codes3) == 1)

    codes4 <- getICD10StandardCodes(cdm = cdm,
                                    level = "ICD10 SubChapter",
                                    nameStyle = "{concept_name}",
                                    name = "XYZ")
    expect_true(length(codes4) == 0)

    # with details
    codes5 <- getICD10StandardCodes(cdm = cdm,
                                    level = "ICD10 SubChapter",
                                    nameStyle = "{concept_name}",
                                    type = "codelist_with_details")
    expect_equal(colnames(codes5[[1]]),
                 c("name", "concept_id","concept_code",
                   "concept_name", "domain_id", "vocabulary_id"))
    expect_true(!is.null(codes5[[1]]$concept_name))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})

test_that("db without icd10 codes loaded", {
  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backend = backends[i])
    cdm$concept <- cdm$concept |>
      dplyr::filter(vocabulary_id != "ICD10")
    expect_message(codes <- getICD10StandardCodes(cdm = cdm))
    expect_true(length(codes) == 0)
    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})

test_that("expected errors", {
  cdm <- mockVocabRef()
  expect_error(getICD10StandardCodes(cdm = "not a cdm"))
  expect_error(getICD10StandardCodes(cdm = cdm, level = c(
    "Not an ICD10 Chapter"
  )))
  expect_error(getICD10StandardCodes(cdm = cdm, name = 1))
  expect_error(getICD10StandardCodes(cdm = cdm, includeDescendants = "hello"))
  expect_error(getICD10StandardCodes(cdm = cdm, type = "random"))
  expect_error(getICD10StandardCodes(cdm = cdm, nameStyle = "hello"))
})


