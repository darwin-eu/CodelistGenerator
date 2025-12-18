test_that("tests with mock db", {
  skip_on_cran()
  # mock db
  cdm <- mockVocabRef("database")

  codes <- getCandidateCodes(
    cdm = cdm,
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = FALSE
  )

  expect_warning( # not all tables in cdm
  orphan_codes <- summariseOrphanCodes(
    x = omopgenerics::newCodelist(list("msk" = codes$concept_id)),
    cdm = cdm)
  )

  # we should pick up knee osteoarthritis from our achilles tables
  expect_true(all(stringr::str_detect(orphan_codes |>
                                        dplyr::pull("variable_level"), c("4", "5", "4", "5"))))
  expect_equal(orphan_codes |>
                 dplyr::pull("estimate_value"),
               c("400", "200", "400", "200"))
  settings <- omopgenerics::settings(orphan_codes)
  expect_true(all(settings$result_type == "orphan_code_use"))

  #expected error
  expect_error(summariseOrphanCodes(x = "a", cdm = cdm))
  expect_error(summariseOrphanCodes(x = omopgenerics::newCodelist(list("msk" = codes$concept_id)),
                                    cdm = "a"))

  # requires achilles
  cdm$achilles_results <- NULL
  expect_error(summariseOrphanCodes(x = list("msk" = codes$concept_id),
                                    cdm = cdm))

  CDMConnector::cdmDisconnect(cdm)

  cdm <- omock::mockCdmReference()
  expect_error(summariseOrphanCodes(x = list("a" = 1), cdm))
})
