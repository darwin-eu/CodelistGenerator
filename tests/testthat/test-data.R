test_that("load route data", {
  expect_identical(colnames(doseFormToRoute),
                   c("dose_form_concept_id",
                     "dose_form_concept_name",
                     "route_category"))
})
