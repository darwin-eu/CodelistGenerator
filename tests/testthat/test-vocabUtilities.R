test_that("tests with mock db", {
  skip_on_cran()
  backends <- c("database", "data_frame")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])
    namesInitial <- names(cdm)
    codelist_with_details <- list("codes" = cdm[["concept"]] |>
                                    dplyr::filter(concept_id %in% c(10L, 15L, 24L)) |>
                                    dplyr::select("concept_id", "concept_name", "domain_id", "vocabulary_id") |>
                                    dplyr::mutate("ancestor_concept_id" = 999L) |>
                                    dplyr::collect()) |>
      omopgenerics::newCodelistWithDetails()

    version <- vocabularyVersion(cdm = cdm)
    expect_true(length(version) == 1)
    expect_true(is.character(version))

    descendants1 <- getDescendants(
      cdm = cdm,
      conceptId = 1,
      withAncestor = FALSE
    )
    expect_true(all(sort(descendants1$concept_id) == c(1, 2, 3, 4, 5)))
    expect_true("concept_name" %in% colnames(descendants1))

    descendants2 <- getDescendants(
      cdm = cdm,
      conceptId = 1,
      withAncestor = TRUE
    )
    expect_true(all(sort(descendants2$concept_id) == c(1, 2, 3, 4, 5)))
    expect_true("ancestor_concept_id" %in% colnames(descendants2))
    expect_true(all(descendants2$ancestor_concept_id == 1))

    # expected errors
    expect_error(vocabularyVersion(cdm = "a"))
    expect_error(getDescendants(cdm = "a"))
    expect_error(availableDoseForm(cdm = "a"))
    expect_error(availableDoseForm(cdm, x = 123))
    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})
