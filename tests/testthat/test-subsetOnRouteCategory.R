test_that("subset on route category", {

  backends <- c("database", "data_frame")

  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backends[[i]])

    # codelist
    codelist <- newCodelist(list("codes1" = c(10L, 14L),
                                 "codes2" = c(11L, 12L)))
    expect_warning(x <- subsetOnRouteCategory(codelist, cdm, routeCategory = c("injectable"), negate = FALSE))
    expect_true(inherits(x, "codelist"))
    expect_true(x$codes1 == 10L)
    expect_no_error(x <- subsetOnRouteCategory(codelist, cdm, routeCategory = c("injectable"), negate = TRUE))
    expect_true(inherits(x, "codelist"))
    expect_true(x$codes1 == c(14L))
    expect_identical(x$codes2, c(11L, 12L))
    expect_no_error(x1 <- subsetOnRouteCategory(codelist, cdm, routeCategory = c("unclassified route category"), negate = FALSE))
    expect_true(inherits(x1, "codelist"))
    expect_identical(x1, x)

    # codelist with details
    codelist_wd <- codelist |> asCodelistWithDetails(cdm)
    expect_warning(x <- subsetOnRouteCategory(codelist_wd,  cdm, "injectable"))
    expect_true(inherits(x, "codelist_with_details"))
    expect_identical(x$codes1 |> dplyr::pull("concept_id"), 10L)

    # check empty codelist
    expect_warning(x <- subsetOnRouteCategory(codelist, cdm, "oral"))
    expect_true(length(x) == 0)
    expect_true(inherits(x, "codelist"))

    # check empty codelist_with_details
    expect_warning(x <- subsetOnRouteCategory(codelist_wd, cdm, "oral"))
    expect_true(length(x) == 0)
    expect_true(inherits(x, "codelist_with_details"))

    # expected errors
    expect_error(subsetOnRouteCategory(list("a" = 1L),  cdm, "oral"))
    expect_error(subsetOnRouteCategory(codelist,  "a", "oral"))
    expect_error(subsetOnRouteCategory(codelist,  cdm, 1234L))

    # Check lower case
    codelist <- newCodelist(list("a" = c(21L, 22L)))
    topical1 <- subsetOnRouteCategory(codelist, cdm, routeCategory = "topical")
    topical3 <- subsetOnRouteCategory(codelist, cdm, routeCategory = "Topical")
    expect_equal(topical1, topical3)

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})

