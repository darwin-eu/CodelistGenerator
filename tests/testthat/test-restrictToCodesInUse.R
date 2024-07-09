test_that("restrict to codes in use", {
  cdm <- mockVocabRef("database")
  startCl <- list(a = c(4,5,6),
                  b = c(1,2))
  endCl <- restrictToCodesInUse(startCl, cdm)
  endCl2 <- subsetToCodesInUse(startCl, cdm)
  expect_identical(endCl, endCl2)

  expect_true(all(c(4,5) %in% endCl[["a"]]))
  expect_false(c(6) %in% endCl[["a"]])
  # b will have been dropped
  expect_true(length(endCl) == 1)


  # restrict on minimum count
  endCl2 <- restrictToCodesInUse(startCl, cdm, minimumCount = 300)
  expect_true(c(4) %in% endCl2[["a"]])
  expect_false(c(5) %in% endCl2[["a"]])
  expect_false(c(6) %in% endCl2[["a"]])

  # none with sufficient count
  endCl3 <- restrictToCodesInUse(startCl, cdm, minimumCount = 500)
  expect_true(length(endCl3) == 0)

})
