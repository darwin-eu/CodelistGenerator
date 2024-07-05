test_that("load route data", {
  expect_identical(colnames(doseFormToRoute),
                   c("dose_form_concept_id", "route_category"))
})
