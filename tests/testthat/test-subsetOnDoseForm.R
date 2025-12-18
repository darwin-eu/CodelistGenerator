test_that("subsetOnDoseForm works", {

  cdm <- mockVocabRef()

  codelist <- newCodelist(list("codes" = c(10L,20L,21L,22L)))

  expect_no_error(codes <- subsetOnDoseForm(
    x = codelist,
    cdm = cdm,
    doseForm = "Injection"))

  expect_true(inherits(codes, "codelist"))
  expect_true(names(codes) == "codes")
  expect_identical(codes$codes, 10L)

  # codelist + negate
  expect_no_error(codes <- subsetOnDoseForm(
    x = codelist,
    cdm = cdm,
    doseForm = "Injection",
    negate = TRUE))

  expect_true(inherits(codes, "codelist"))
  expect_true(names(codes) == "codes")
  expect_identical(codes$codes, c(20L, 21L,22L))

  # codelist with details
  codelist <- asCodelistWithDetails(codelist, cdm)

  expect_no_error(codes <- subsetOnDoseForm(
    x = codelist,
    cdm = cdm,
    doseForm = c("Injection","Nasal Powder")))

  expect_true(inherits(codes, "codelist_with_details"))
  expect_true(names(codes) == "codes")
  expect_identical(codes$codes,
                   dplyr::tibble("concept_id" = c(10L, 20L),
                                 "concept_name" = c("Adalimumab", "glucagon Nasal Powder"),
                                 "domain_id" = rep("Drug",2),
                                 "vocabulary_id" = rep("RxNorm",2),
                                 "standard_concept" = rep("standard",2)))

  # codelist + negate
  expect_no_error(codes <- subsetOnDoseForm(
    x = codelist,
    cdm = cdm,
    doseForm = c("Injection"),
    negate = TRUE))

  expect_true(inherits(codes, "codelist_with_details"))
  expect_true(names(codes) == "codes")
  expect_identical(codes$codes |> dplyr::pull("concept_id"), c(20L, 21L,22L))

  # Empty codelist
  expect_warning(codes <- subsetOnDoseForm(
    x = codelist,
    cdm = cdm,
    doseForm = c("a"),
    negate = FALSE))

  # Multiple elements
  codelist <- omopgenerics::newCodelist(list("codes1" = c(10L,20L), "codes2" = c(21L,22L)))
  expect_no_error(codes <- subsetOnDoseForm(
    x = codelist,
    cdm = cdm,
    doseForm = c("Injection", "Topical Liquefied Gas"),
    negate = FALSE))
  expect_true(inherits(codes, "codelist"))
  expect_identical(names(codes), c("codes1", "codes2"))

  # lower case
  expect_no_error(codes1 <- subsetOnDoseForm(
    x = codelist,
    cdm = cdm,
    doseForm = c("injection", "topical liquefied gas"),
    negate = FALSE))
  expect_identical(codes, codes1)

  # check empty codelists
  x <- subsetOnDoseForm(omopgenerics::emptyCodelist(), cdm, doseForm = "Injection")
  expect_true(inherits(x, "codelist"))
  x <- subsetOnDoseForm(omopgenerics::emptyCodelistWithDetails(), cdm, doseForm = "Injection")
  expect_true(inherits(x, "codelist_with_details"))

  # expected errors
  expect_error(subsetOnDoseForm(list("a" = 1L), cdm, doseForm = "Injection"))
  expect_error(subsetOnDoseForm(codelist, "a", doseForm = "Injection"))
  expect_error(subsetOnDoseForm(codelist, cdm, doseForm = 1L))
  expect_error(subsetOnDoseForm(codelist, cdm, doseForm = "Injection", negate = "a"))

})
