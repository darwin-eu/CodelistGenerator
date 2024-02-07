test_that("achilles code use", {

  # mock db
  cdm <- mockVocabRef("database")
  oa <- getCandidateCodes(cdm = cdm, keywords = "osteoarthritis")
  # two codes: "Osteoarthritis of knee" "Osteoarthritis of hip"
  result_achilles <- achillesCodeUse(list(oa = oa$concept_id),
                                     cdm = cdm)
  expect_true(result_achilles %>%
                dplyr::filter(group_level == 4) %>%
    dplyr::pull("estimate_value") == 400)
  expect_true(result_achilles %>%
                dplyr::filter(group_level == 5) %>%
                dplyr::pull("estimate_value") ==200)
  expect_true(nrow(result_achilles) == 2)
  expect_equal(c("oa", "oa"),
               result_achilles %>%
                 dplyr::pull("strata_name"))

  # check is a summarised result
  # expect_true("summarised_result" %in%  class(result_achilles))

  # applying min cell count where estimate should be obscured
  result_achilles <- achillesCodeUse(list(oa = oa$concept_id),
                                     cdm = cdm,
                                     minCellCount = 500)
  expect_true(all(is.na(result_achilles %>%
                dplyr::pull("estimate_value"))))


 # edge cases
 # concept id not in achilles
 expect_message(result_achilles <- achillesCodeUse(list(asthma = 123),
                                    cdm = cdm))
 expect_true(nrow(result_achilles) == 0)
 # expect_true("summarised_result" %in%  class(result_achilles))

 # expected errors
 expect_error(achillesCodeUse(123, #not a named list
                 cdm = cdm))
 expect_error(achillesCodeUse(asthma,
                 cdm = "cdm")) # not a cdm
 expect_error(achillesCodeUse(asthma,
                 cdm = cdm,
                 countBy = "not an option"))
 expect_error(achillesCodeUse(asthma,
                 cdm = cdm,
                 minCellCount = "not a number"))

 CDMConnector::cdm_disconnect(cdm)
})

test_that("achilles code use: multipe codelists", {

  # mock db
  cdm <- mockVocabRef("database")

  # two codelists: "Osteoarthritis of knee" "Osteoarthritis of hip"
  result_achilles <- achillesCodeUse(list(knee_oa = 4,
                                          hip_oa = 5),
                                     cdm = cdm)

  expect_true(result_achilles %>%
                dplyr::filter(group_level == 4) %>%
                dplyr::pull("estimate_value") == "400")
  expect_true(result_achilles %>%
                dplyr::filter(group_level == 5) %>%
                dplyr::pull("estimate_value") == "200")
  expect_true(nrow(result_achilles) == 2)
  expect_equal(c("knee_oa", "hip_oa"),
               result_achilles %>%
                dplyr::pull("strata_name"))

  CDMConnector::cdm_disconnect(cdm)
})
