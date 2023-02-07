test_that("tests with mock db", {
  backends <- c("database", "arrow", "data_frame")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])

    version <- getVocabVersion(cdm = cdm)
    expect_true(length(version) == 1)
    expect_true(is.character(version))

    vocabs <- getVocabularies(cdm = cdm)
    expect_true(length(vocabs) >= 1)
    expect_true(is.character(vocabs))

    domains <- getDomains(cdm = cdm)
    expect_true(all(c("Condition", "Observation") %in% domains))
    expect_true(is.character(domains))

    conceptClasses <- getConceptClassId(cdm = cdm)
    expect_true(is.character(conceptClasses))

    conceptClasses <- getConceptClassId(
      cdm = cdm,
      domain = "Condition"
    )
    expect_true(is.character(conceptClasses))

    descendants <- getDescendants(
      cdm = cdm,
      conceptId = 1
    )
    expect_true(all(descendants$concept_id == c(1, 2, 3, 4, 5)))


    doseForms <- getDoseForm(cdm = cdm)
    expect_true(doseForms == "Injection")

    # expected errors
    expect_error(getVocabVersion(cdm = "a"))
    expect_error(getVocabularies(cdm = "a"))
    expect_error(getDomains(cdm = "a"))
    expect_error(getDomains(cdm, standardConcept = FALSE))
    expect_error(getConceptClassId(cdm = "a"))
    expect_error(getConceptClassId(cdm, standardConcept = FALSE))
    expect_error(getDescendants(cdm = "a"))
    expect_error(getDoseForm(cdm = "a"))

    if (backends[[i]] == "database") {
      DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
    }
  }
})
