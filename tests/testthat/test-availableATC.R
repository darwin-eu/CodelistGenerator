test_that("available ATC", {
  skip_on_cran()
  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backends[[i]])

    expect_no_error(
      availableATC(cdm)
    )
    expect_no_error(
      availableATC(cdm, level = c("ATC 2nd"))
    )
    expect_error(
      availableATC(cdm, level = c("ATC 3nd"))
    )

    expect_no_error(res1 <- availableATC(cdm, level = c("ATC 1st", "ATC 2nd")))
    expect_true(res1 == "ALIMENTARY TRACT AND METABOLISM")
    expect_no_error(res2 <- availableATC(cdm, level = c("ATC 1st")))
    expect_true(setequal(intersect(res2, res1), res2)) # res 2 is a subset of res

    expect_error(availableATC("cdm"))
    expect_error(availableATC(cdm, level = "a"))
  }
})
