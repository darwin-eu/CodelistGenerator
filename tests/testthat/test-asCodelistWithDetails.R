test_that("asCodelistWithDetails", {
  skip_on_cran()
  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backends[[i]])

    c <- omopgenerics::newCodelist(list("codes" = c(12L,13L),
                                        "codes_2" = c(12L)))

    # codelist to codelist with details
    expect_no_error(c_1 <- c |>
                      asCodelistWithDetails(cdm))
    expect_identical(names(c), names(c_1))
    expect_equal(c(12L, 13L),
      sort(c_1[[1]] |> dplyr::pull("concept_id")))
    expect_equal(c(12L),
                 sort(c_1[[2]] |> dplyr::pull("concept_id")))

    # candidate_codes to codelist_with_details
    a <- getCandidateCodes(cdm, keywords = "osteo")
    c <- asCodelistWithDetails(a)
    expect_true(inherits(c, "codelist_with_details"))
    expect_true(!"candidate_codes" %in% class(c[[1]]))

    expect_no_error(omopgenerics::emptyCodelist() |>
      asCodelistWithDetails(cdm))
    expect_no_error(omopgenerics::emptyCodelistWithDetails() |>
                      asCodelistWithDetails())

  }
})

