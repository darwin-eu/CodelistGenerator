test_that("achilles code use", {
  # mock db
  cdm <- mockVocabRef("database")

  expect_message(summariseAchillesCodeUse(x = omopgenerics::emptyCodelist(), cdm))

  oa <- getCandidateCodes(cdm = cdm, keywords = "osteoarthritis")
  # two codes: "Osteoarthritis of knee" "Osteoarthritis of hip"
  result_achilles <- summariseAchillesCodeUse(list(oa = oa$concept_id),
                                     cdm = cdm)
  expect_true(result_achilles |>
                dplyr::filter(stringr::str_detect(variable_level, "4")) |>
    dplyr::pull("estimate_value") == "400")
  expect_true(result_achilles |>
                dplyr::filter(stringr::str_detect(variable_level, "5")) |>
                dplyr::pull("estimate_value") == "200")
  expect_true(nrow(result_achilles) == 2)
  expect_equal(c("oa", "oa"),
               result_achilles |>
                 dplyr::pull("group_level"))

  # check is a summarised result
  expect_true("summarised_result" %in%  class(result_achilles))

  # applying min cell count where estimate should be obscured
  result_achilles <- summariseAchillesCodeUse(list(oa = oa$concept_id),
                                     cdm = cdm)
  expect_true(all(result_achilles |>
                          omopgenerics::suppress(minCellCount = 500) |>
                dplyr::pull("estimate_value") == "-"
                ))


 # edge cases
 # concept id not in achilles
 expect_message(result_achilles <- summariseAchillesCodeUse(list(asthma = 123),
                                    cdm = cdm))
 expect_true(nrow(result_achilles) == 0)
 # expect_true("summarised_result" %in%  class(result_achilles))

 # expected errors
 expect_error(summariseAchillesCodeUse(123, #not list
                 cdm = cdm))
 expect_error(summariseAchillesCodeUse(list(123), #not named list
                                       cdm = cdm))
 expect_error(summariseAchillesCodeUse(asthma,
                 cdm = "cdm")) # not a cdm
 expect_error(summariseAchillesCodeUse(asthma,
                 cdm = cdm,
                 countBy = "not an option"))

 CDMConnector::cdmDisconnect(cdm)

 cdm <- omock::mockCdmReference()
 expect_error(summariseAchillesCodeUse(x = list("a" = 1), cdm))
})

test_that("achilles code use: multipe codelists", {

  # mock db
  cdm <- mockVocabRef("database")

  # two codelists: "Osteoarthritis of knee" "Osteoarthritis of hip"
  result_achilles <- summariseAchillesCodeUse(list(knee_oa = 4,
                                          hip_oa = 5),
                                     cdm = cdm)

  expect_true(result_achilles |>
                dplyr::filter(group_level == "knee_oa") |>
                dplyr::pull("estimate_value") == "400")
  expect_true(result_achilles |>
                dplyr::filter(group_level == "hip_oa") |>
                dplyr::pull("estimate_value") == "200")
  expect_true(result_achilles |>
                dplyr::filter(stringr::str_detect(variable_level, "4")) |>
                dplyr::pull("estimate_value") == "400")
  expect_true(result_achilles |>
                dplyr::filter(stringr::str_detect(variable_level, "5")) |>
                dplyr::pull("estimate_value") == "200")
  expect_true(nrow(result_achilles) == 2)
  expect_equal(c("knee_oa", "hip_oa"),
               result_achilles |>
                dplyr::pull("group_level"))

  CDMConnector::cdmDisconnect(cdm)
})
