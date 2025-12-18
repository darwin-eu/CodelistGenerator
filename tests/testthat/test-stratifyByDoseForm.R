test_that("stratifyByDoseForm works", {
  cdm <- mockVocabRef()
  n1 <- names(cdm)

  # codelist
  codes <- newCodelist(list("codes1" = c(10L, 20L),
                            "codes2" = c(21L, 22L)))
  expect_no_error(new_codes <- stratifyByDoseForm(x = codes,
                                                  cdm = cdm,
                                                  keepOriginal = FALSE))
  expect_equal(names(cdm), n1)
  expect_true(inherits(new_codes, "codelist"))
  expect_equal(names(new_codes), c(
    "codes1_injection", "codes1_nasal_powder",
    "codes2_topical_liquefied_gas", "codes2_unclassified_dose_form"
  ))
  expect_true(new_codes$codes1_injection == 10L)
  expect_true(new_codes$codes1_nasal_powder == 20L)
  expect_true(new_codes$codes2_topical_liquefied_gas == 21L)
  expect_true(new_codes$codes2_unclassified_dose_form == 22L)

  # codelist with keep original
  expect_no_error(new_codes <- stratifyByDoseForm(x = codes,
                                                   cdm = cdm,
                                                   keepOriginal = TRUE))
  expect_equal(names(new_codes), c(
    "codes1","codes1_injection", "codes1_nasal_powder", "codes2",
    "codes2_topical_liquefied_gas", "codes2_unclassified_dose_form"
  ))

  # codelist with details
  codes <- codes |> asCodelistWithDetails(cdm)
  expect_no_error(new_codes <- stratifyByDoseForm(x = codes,
                                                   cdm = cdm,
                                                   keepOriginal = FALSE))
  expect_equal(names(cdm), n1)
  expect_true(inherits(new_codes, "codelist_with_details"))
  expect_equal(names(new_codes), c(
    "codes1_injection", "codes1_nasal_powder",
    "codes2_topical_liquefied_gas", "codes2_unclassified_dose_form"
  ))
  expect_equal(new_codes$codes1_injection,
               dplyr::tibble("concept_id" = 10L,
                             "concept_name" = "Adalimumab",
                             "domain_id" = "Drug",
                             "vocabulary_id" = "RxNorm",
                             "standard_concept" = "standard"))

  # codelist with details + keep originals
  expect_no_error(new_codes <- stratifyByDoseForm(x = codes,
                                                  cdm = cdm,
                                                  keepOriginal = TRUE))
  expect_equal(names(new_codes),
               c("codes1", "codes1_injection", "codes1_nasal_powder",
                 "codes2", "codes2_topical_liquefied_gas", "codes2_unclassified_dose_form"))

  # expected errors
  expect_error(stratifyByDoseForm(x = list("a" = 1L), cdm))
  expect_error(stratifyByDoseForm(x = codes, "a"))
  expect_error(stratifyByDoseForm(x = codes, cdm, keepOriginal = "a"))

})
