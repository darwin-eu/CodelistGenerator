# Testing against different database platforms


test_that("achilles code use", {

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

  cdm$achilles_results <- cdm$condition_occurrence %>%
    dplyr::group_by(condition_concept_id) %>%
    dplyr::tally(name = "count_value") %>%
    dplyr::rename("stratum_1" = "condition_concept_id") %>%
    dplyr::mutate(stratum_2 = NA,
                  stratum_3 = NA,
                  analysis_id = 401) %>%
    dplyr::compute()

  asthma <- list(asthma = c(317009, 257581))
  result_achilles <- achillesCodeUse(asthma,
                                     cdm = cdm)
  result_cdm <- summariseCodeUse(asthma, cdm = cdm)

  expect_equal(as.numeric(result_achilles %>%
                 dplyr::filter(group_level == "317009",
                               group_name == "by_concept",
                               variable_name == "Record count") %>%
                 dplyr::pull("estimate_value")),
               result_cdm %>%
                 dplyr::filter(standard_concept_id == 317009,
                               group_name == "By concept",
                               variable_name == "Record count") %>%
                 dplyr::pull("estimate"))

  expect_equal(as.numeric(result_achilles %>%
                 dplyr::filter(group_level == "257581",
                               group_name == "by_concept",
                               variable_name == "Record count") %>%
                 dplyr::pull("estimate_value")),
               result_cdm %>%
                 dplyr::filter(standard_concept_id == 257581,
                               group_name == "By concept",
                               variable_name == "Record count") %>%
                 dplyr::pull("estimate"))


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
    CDMConnector::computeQuery()

  asthma <- list(asthma = c(317009, 257581))
  result_achilles <- achillesCodeUse(asthma,
                                     cdm = cdm)
  result_cdm <- summariseCodeUse(asthma, cdm = cdm)


  expect_equal(as.numeric(result_achilles %>%
                 dplyr::filter(group_level == "317009",
                               group_name == "by_concept",
                               variable_name == "Person count") %>%
                 dplyr::pull("estimate_value")),
               result_cdm %>%
                 dplyr::filter(standard_concept_id == 317009,
                               group_name == "By concept",
                               variable_name == "Person count") %>%
                 dplyr::pull("estimate"))

  expect_equal(as.numeric(result_achilles %>%
                            dplyr::filter(group_level == "257581",
                                          group_name == "by_concept",
                                          variable_name == "Person count") %>%
                            dplyr::pull("estimate_value")),
               result_cdm %>%
                 dplyr::filter(standard_concept_id == 257581,
                               group_name == "By concept",
                               variable_name == "Person count") %>%
                 dplyr::pull("estimate"))

  # edge cases
  # concept id not in achilles
  expect_message(achillesCodeUse(list(asthma = 123),
                                 cdm = cdm))

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

