# Testing against different database platforms


test_that("redshift", {

  testthat::skip_if(Sys.getenv("CDM5_REDSHIFT_DBNAME") == "")

  db <-  DBI::dbConnect(RPostgres::Redshift(),
                        dbname   = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
                        host     = Sys.getenv("CDM5_REDSHIFT_HOST"),
                        port     = Sys.getenv("CDM5_REDSHIFT_PORT"),
                        user     = Sys.getenv("CDM5_REDSHIFT_USER"),
                        password = Sys.getenv("CDM5_REDSHIFT_PASSWORD"))

  cdm <- CDMConnector::cdm_from_con(con = db,
                                    cdm_schema = Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA"),
                                    write_schema = Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA"))


  # candidate code search
  expect_no_error(asthma<-getCandidateCodes(cdm,
                                            keywords = c("asthma",
                                                         "irritable airways",
                                                         "lung disease",
                                                         "respiratory abnormalities",
                                                         "sleep apnea",
                                                         "chronic obstructive lung disease",
                                                         "chronic obstructive lung disease"),

                       domains = c("condition", "observation"),
                       exclude = c("childhood", "juvenile"),
                       searchInSynonyms = TRUE,
                       searchNonStandard = TRUE,
                       includeDescendants = TRUE,
                       includeAncestor = TRUE))
  expect_true(nrow(asthma) > 0)

  # achilles
  cdm$achilles_results <- cdm$condition_occurrence %>%
    dplyr::group_by(condition_concept_id) %>%
    dplyr::tally(name = "count_value") %>%
    dplyr::rename("stratum_1" = "condition_concept_id") %>%
    dplyr::mutate(stratum_2 = NA,
                  stratum_3 = NA,
                  analysis_id = 401) %>%
    dplyr::compute()

  asthma <- list(asthma = c(317009, 257581))
  result_achilles <- summariseAchillesCodeUse(asthma,
                                     cdm = cdm)
  result_cdm <- summariseCodeUse(asthma, cdm = cdm)

  expect_equal(result_achilles %>%
                 dplyr::filter(stringr::str_detect(variable_level, "317009"),
                               estimate_name == "record_count") %>%
                 dplyr::pull("estimate_value"),
               result_cdm %>%
                 dplyr::filter(variable_level == 317009,
                               estimate_name == "record_count") %>%
                 dplyr::pull("estimate_value"))

  expect_equal(result_achilles %>%
                 dplyr::filter(stringr::str_detect(variable_level, "257581"),
                               estimate_name == "record_count") %>%
                 dplyr::pull("estimate_value"),
               result_cdm %>%
                 dplyr::filter(variable_level == 257581,
                               estimate_name == "record_count") %>%
                 dplyr::pull("estimate_value"))


  cdm$achilles_results <- cdm$condition_occurrence %>%
    dplyr::group_by(person_id, condition_concept_id) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(condition_concept_id) %>%
    dplyr::tally(name = "count_value") %>%
    dplyr::rename("stratum_1" = "condition_concept_id") %>%
    dplyr::mutate(stratum_2 = NA,
                  stratum_3 = NA,
                  analysis_id = 400) %>%
    dplyr::compute()

  asthma <- list(asthma = c(317009, 257581))
  result_achilles <- summariseAchillesCodeUse(asthma,
                                     cdm = cdm)
  result_cdm <- summariseCodeUse(asthma, cdm = cdm)

  expect_equal(result_achilles %>%
                 dplyr::filter(stringr::str_detect(variable_level, "317009"),
                               estimate_name == "person_count") %>%
                 dplyr::pull("estimate_value"),
               result_cdm %>%
                 dplyr::filter(variable_level == 317009,
                               estimate_name == "person_count") %>%
                 dplyr::pull("estimate_value"))

  expect_equal(result_achilles %>%
                 dplyr::filter(stringr::str_detect(variable_level, "257581"),
                               estimate_name == "person_count") %>%
                 dplyr::pull("estimate_value"),
               result_cdm %>%
                 dplyr::filter(variable_level == 257581,
                               estimate_name == "person_count") %>%
                 dplyr::pull("estimate_value"))


  # edge cases
  # concept id not in achilles
  expect_message(summariseAchillesCodeUse(list(asthma = 123),
                                 cdm = cdm))

  # expected errors
  expect_error(summariseAchillesCodeUse(123, #not a named list
                               cdm = cdm))
  expect_error(summariseAchillesCodeUse(asthma,
                               cdm = "cdm")) # not a cdm
  expect_error(summariseAchillesCodeUse(asthma,
                               cdm = cdm,
                               countBy = "not an option"))

  CDMConnector::cdm_disconnect(cdm)
})

