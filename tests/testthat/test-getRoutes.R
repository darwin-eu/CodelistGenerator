test_that("getRoutes works with categories", {


  cdm <- mockVocabRef()
  expect_true(is.character(getRoutes(cdm)))

  expect_true(all(getRoutes(cdm, category = TRUE) %in%
                    doseFormToRoute$route_category))

  expect_identical(getRoutes(cdm),
                   unique(getRoutes(cdm)))
  expect_identical(getRoutes(cdm),
                   sort(getRoutes(cdm)))



})
