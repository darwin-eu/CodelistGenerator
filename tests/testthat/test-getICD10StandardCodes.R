test_that("db without icd10 codes loaded", {
  cdm <- mockVocabRef()
  codes <- getICD10StandardCodes(cdm = cdm,
                                 level = c(
                                   "ICD10 Chapter",
                                   "ICD10 SubChapter"
                                 ))
  expect_true(length(codes) == 2)
  # we should pick up mapping and descendants
  expect_true(all(codes[[1]] %in% c(3,4,5)))
  expect_true(all(codes[[2]] %in% c(3,4,5)))


  # with only sub-chapter
  codes2 <- getICD10StandardCodes(cdm = cdm,
                                 level = "ICD10 SubChapter")
  expect_true(length(codes2) == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("db without icd10 codes loaded", {
  cdm <- mockVocabRef()
  cdm$concept <- cdm$concept %>%
    dplyr::filter(vocabulary_id != "ICD10")
  expect_message(codes <- getICD10StandardCodes(cdm = cdm))
  expect_true(length(codes) == 0)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("expected errors", {
  cdm <- mockVocabRef()
  expect_error(getICD10StandardCodes(cdm = "not a cdm"))
  expect_error(getICD10StandardCodes(cdm = cdm, level = c(
    "Not an ICD10 Chapter"
  )))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
