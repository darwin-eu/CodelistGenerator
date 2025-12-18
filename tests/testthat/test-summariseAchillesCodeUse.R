test_that("achilles code use", {
  skip_on_cran()
  # mock db
  cdm <- mockVocabRef("database")

  expect_message(summariseAchillesCodeUse(x = omopgenerics::emptyCodelist(), cdm))

  oa <- getCandidateCodes(cdm = cdm, keywords = "osteoarthritis")
  # two codes: "Osteoarthritis of knee" "Osteoarthritis of hip"
  result_achilles <- summariseAchillesCodeUse(omopgenerics::newCodelist(list(oa = oa$concept_id)),
                                              cdm = cdm)
  expect_true(result_achilles |>
                dplyr::filter(estimate_name == "record_count") |>
                dplyr::filter(stringr::str_detect(variable_level, "4")) |>
                dplyr::pull("estimate_value") == "400")
  expect_true(result_achilles |>
                dplyr::filter(estimate_name == "record_count") |>
                dplyr::filter(stringr::str_detect(variable_level, "5")) |>
                dplyr::pull("estimate_value") == "200")
  expect_true(nrow(result_achilles) == 4)
  expect_equal(c(rep("oa",4)),
               result_achilles |>
                 dplyr::pull("group_level"))

  # check is a summarised result
  expect_true("summarised_result" %in%  class(result_achilles))

  # applying min cell count where estimate should be obscured
  result_achilles <- summariseAchillesCodeUse(omopgenerics::newCodelist(list(oa = oa$concept_id)),
                                              cdm = cdm)
  expect_true(all(result_achilles |>
                    omopgenerics::suppress(minCellCount = 500) |>
                    dplyr::pull("estimate_value") == "-"
  ))


  # edge cases
  # concept id not in achilles
  expect_message(result_achilles <- summariseAchillesCodeUse(omopgenerics::newCodelist(list(asthma = 123L)),
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
  result_achilles <- summariseAchillesCodeUse(omopgenerics::newCodelist(
    list(knee_oa = 4L,
         hip_oa = 5L)),
    cdm = cdm)

  expect_true(result_achilles |>
                dplyr::filter(group_level == "knee_oa") |>
                dplyr::filter(estimate_name == "record_count") |>
                dplyr::pull("estimate_value") == "400")
  expect_true(result_achilles |>
                dplyr::filter(group_level == "hip_oa") |>
                dplyr::filter(estimate_name == "person_count") |>
                dplyr::pull("estimate_value") == "200")
  expect_true(result_achilles |>
                dplyr::filter(stringr::str_detect(variable_level, "4")) |>
                dplyr::filter(estimate_name == "person_count") |>
                dplyr::pull("estimate_value") == "400")
  expect_true(result_achilles |>
                dplyr::filter(stringr::str_detect(variable_level, "5")) |>
                dplyr::filter(estimate_name == "record_count") |>
                dplyr::pull("estimate_value") == "200")
  expect_true(nrow(result_achilles) == 4)
  expect_equal(rep(c("hip_oa","knee_oa"),2),
               result_achilles |>
                 dplyr::pull("group_level"))

  CDMConnector::cdmDisconnect(cdm)
})
