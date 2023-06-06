test_that("test inputs", {
  cdm <- mockDrugUtilisation(connectionDetails)
  expect_error(readConceptList())
  expect_error(readConceptList(cdm = cdm))
  expect_error(readConceptList(cdm = cdm, path = 1))
  expect_error(readConceptList(cdm = cdm, path = "not/a/path"))
  x <- readConceptList(
    cdm = cdm, path =  system.file(package = "CodelistGenerator", "concepts")
  )
  expect_true(typeof(x) == "list")
  expect_true(all(names(x) %in% c("influenza", "acetaminophen")))
  expect_true(x$influenza == 4266367)
  expect_true(all(x$acetaminophen %in% c(1125315, 43135274, 2905077, 1125360)))
})
