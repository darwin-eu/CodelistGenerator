test_that("achilles code use", {

  # mock db
  cdm <- mockVocabRef("database")

  oa <- getCandidateCodes(cdm = cdm, keywords = "osteoarthritis")
  # two codes: "Osteoarthritis of knee" "Osteoarthritis of hip"
  # in achilles we only have a count for "Osteoarthritis of knee"
  result_achilles <- achillesCodeUse(list(oa = oa$concept_id),
                                     cdm = cdm)
  expect_true(result_achilles %>%
    dplyr::pull("estimate") == 100)

  # applying min cell count where estimate should be obscured
  result_achilles <- achillesCodeUse(list(oa = oa$concept_id),
                                     cdm = cdm,
                                     minCellCount = 150)
  expect_true(is.na(result_achilles %>%
                dplyr::pull("estimate")))


 # edge cases
 # concept id not in achilles
 expect_message(result_achilles <- achillesCodeUse(list(asthma = 123),
                                    cdm = cdm))
 expect_true(nrow(result_achilles) == 0)

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
