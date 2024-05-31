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

  cdm$concept <- cdm$concept |>
    dplyr::mutate(concept_id = as.integer64(concept_id)) |>
    dplyr::compute()

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

  # drug ingredients
  expect_no_error(metformin <- getDrugIngredientCodes(cdm, "metformin"))
  expect_true(is.integer(metformin$metformin))

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

  # working concept set example with mock
  x <- codesFromConceptSet(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "concepts_dbms")
  )
  expect_true(x$oa_no_desc == 4079750)
  expect_true(!761485 %in% x$oa_no_desc)
  expect_true(761485 %in% x$oa_desc)


  CDMConnector::cdm_disconnect(cdm)
})

test_that("snowflake", {

  testthat::skip_if(Sys.getenv("SNOWFLAKE_SERVER") == "")

  con <- DBI::dbConnect(odbc::odbc(),
                        SERVER = Sys.getenv("SNOWFLAKE_SERVER"),
                        UID = Sys.getenv("SNOWFLAKE_USER"),
                        PWD = Sys.getenv("SNOWFLAKE_PASSWORD"),
                        DATABASE = Sys.getenv("SNOWFLAKE_DATABASE"),
                        WAREHOUSE = Sys.getenv("SNOWFLAKE_WAREHOUSE"),
                        DRIVER = Sys.getenv("SNOWFLAKE_DRIVER"))

  cdm_schema <- strsplit(Sys.getenv("SNOWFLAKE_CDM_SCHEMA"), "\\.")[[1]]
  write_schema <- strsplit(Sys.getenv("SNOWFLAKE_SCRATCH_SCHEMA"), "\\.")[[1]]

  cdm <- CDMConnector::cdm_from_con(con = con,
                                    cdm_schema = cdm_schema,
                                    write_schema = write_schema,
                                    cdm_name = "snowflake")


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

  # drug ingredients
  expect_no_error(getDrugIngredientCodes(cdm, "metformin"))

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
                            dplyr::filter(variable_level == "317009",
                                          variable_name == "record_count") %>%
                            dplyr::pull("estimate_value"),
               result_cdm %>%
                 dplyr::filter(variable_level == "317009",
                               variable_name == "record_count ") %>%
                 dplyr::pull("estimate_value"))

  expect_equal(result_achilles %>%
                 dplyr::filter(variable_level == "257581",
                               variable_name == "record_count") %>%
                 dplyr::pull("estimate_value"),
               result_cdm %>%
                 dplyr::filter(variable_level == "257581",
                               variable_name == "record_count ") %>%
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
                 dplyr::filter(variable_level == "317009",
                               variable_name == "record_count") %>%
                 dplyr::pull("estimate_value"),
               result_cdm %>%
                 dplyr::filter(variable_level == "317009",
                               variable_name == "record_count ") %>%
                 dplyr::pull("estimate_value"))



  expect_equal(result_achilles %>%
                            dplyr::filter(group_level == "317009",
                                          variable_name == "person_count") %>%
                            dplyr::pull("estimate_value"),
               result_cdm %>%
                 dplyr::filter(group_level ==  "317009",
                               variable_name == "person_count") %>%
                 dplyr::pull("estimate_value"))
  expect_equal(result_achilles %>%
                 dplyr::filter(group_level == "257581",
                               variable_name == "person_count") %>%
                 dplyr::pull("estimate_value"),
               result_cdm %>%
                 dplyr::filter(group_level ==  "257581",
                               variable_name == "person_count") %>%
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
