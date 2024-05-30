# Testing against different database platforms

test_that("redshift", {

  testthat::skip_on_cran()

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



  # test summariseCodeUse
  asthma <- list(asthma = c(317009, 257581))

  results <- summariseCodeUse(asthma,
                              cdm = cdm,
                              byYear = TRUE,
                              bySex = TRUE,
                              ageGroup = list(c(0,17),
                                              c(18,65),
                                              c(66, 100)))
  # column names
  expect_true(inherits(results, "summarised_result"))

  # overall record count
  expect_true(results %>%
                dplyr::filter(variable_name == "overall" &
                                strata_name == "overall" &
                                strata_level == "overall",
                              estimate_name == "record_count") %>%
                dplyr::pull("estimate_value") %>%
                as.numeric() ==
                cdm$condition_occurrence %>%
                dplyr::filter(condition_concept_id %in%  !!asthma[[1]]) %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # overall person count
  expect_true(results %>%
                dplyr::filter(variable_name == "overall" &
                                strata_name == "overall" &
                                strata_level == "overall" &
                                estimate_name == "person_count") %>%
                dplyr::pull("estimate_value") %>%
                as.numeric() ==
                cdm$condition_occurrence %>%
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) %>%
                dplyr::select("person_id") %>%
                dplyr::distinct() %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # by year
  # overall record count
  expect_true(results %>%
                dplyr::filter(variable_name == "overall" &
                                strata_name == "year" &
                                strata_level == "2008",
                              estimate_name == "record_count") %>%
                dplyr::pull("estimate_value") %>%
                as.numeric() ==
                cdm$condition_occurrence %>%
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) %>%
                dplyr::filter(year(condition_start_date) == 2008) %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # overall person count
  expect_true(results %>%
                dplyr::filter(variable_name == "overall" &
                                strata_name == "year" &
                                strata_level == "2008",
                              estimate_name == "person_count") %>%
                dplyr::pull("estimate_value") %>%
                as.numeric() ==
                cdm$condition_occurrence %>%
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) %>%
                dplyr::filter(year(condition_start_date) == 2008) %>%
                dplyr::select("person_id") %>%
                dplyr::distinct() %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # by age group and sex
  # overall record count
  expect_true(results %>%
                dplyr::filter(variable_name == "overall" &
                                strata_name == "sex" &
                                strata_level == "Male",
                              estimate_name == "record_count") %>%
                dplyr::pull("estimate_value") %>%
                as.numeric() ==
                cdm$condition_occurrence %>%
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) %>%
                PatientProfiles::addSex() %>%
                dplyr::filter(sex == "Male") %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  expect_true(results %>%
                dplyr::filter(variable_name == "overall" &
                                strata_name == "age_group &&& sex" &
                                strata_level == "18 to 65 &&& Male",
                              estimate_name == "record_count") %>%
                dplyr::pull("estimate_value") %>%
                as.numeric() ==
                cdm$condition_occurrence %>%
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) %>%
                PatientProfiles::addAge(indexDate = "condition_start_date") %>%
                PatientProfiles::addSex() %>%
                dplyr::filter(sex == "Male" &
                                age >= "18" &
                                age <= "65") %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # overall person count
  expect_true(results %>%
                dplyr::filter(variable_name == "overall" &
                                strata_name == "age_group &&& sex" &
                                strata_level == "18 to 65 &&& Male",
                              estimate_name == "person_count") %>%
                dplyr::pull("estimate_value") %>%
                as.numeric() ==
                cdm$condition_occurrence %>%
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) %>%
                PatientProfiles::addAge(indexDate = "condition_start_date") %>%
                PatientProfiles::addSex() %>%
                dplyr::filter(sex == "Male" &
                                age >= "18" &
                                age <= "65") %>%
                dplyr::select("person_id") %>%
                dplyr::distinct() %>%
                dplyr::tally() %>%
                dplyr::pull("n"))




  results <- summariseCodeUse(asthma,
                              cdm = cdm, countBy = "person",
                              byYear = FALSE,
                              bySex = FALSE,
                              ageGroup = NULL)
  expect_true(nrow(results %>%
                     dplyr::filter(estimate_name == "person_count")) > 0)
  expect_true(nrow(results %>%
                     dplyr::filter(estimate_name == "record_count")) == 0)

  results <- summariseCodeUse(asthma,
                              cdm = cdm, countBy = "record",
                              byYear = FALSE,
                              bySex = FALSE,
                              ageGroup = NULL)
  expect_true(nrow(results %>%
                     dplyr::filter(estimate_name == "person_count")) == 0)
  expect_true(nrow(results %>%
                     dplyr::filter(estimate_name == "record_count")) > 0)


  # domains covered

  # condition
  expect_true(nrow(summariseCodeUse(list(cs = c(317009)),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # visit
  expect_true(nrow(summariseCodeUse(list(cs = 9201),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # drug
  expect_true(nrow(summariseCodeUse(list(cs = 19071493),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # measurement
  expect_true(nrow(summariseCodeUse(list(cs = 2212542),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # procedure and condition
  expect_true(nrow(summariseCodeUse(list(cs = c(4261206,317009)),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # no records
  expect_message(results <- summariseCodeUse(list(cs = c(999999)),
                                             cdm = cdm,
                                             byYear = FALSE,
                                             bySex = FALSE,
                                             ageGroup = NULL))
  expect_true(nrow(results) == 0)



  # expected errors
  expect_error(summariseCodeUse(list(cs = "not a concept"),
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseCodeUse(list(cs = 123),
                                cdm = "not a cdm",
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseCodeUse(list(cs = 123),
                                cdm = cdm,
                                byYear = "Maybe",
                                bySex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseCodeUse(list(cs = 123),
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = "Maybe",
                                ageGroup = NULL))
  expect_error(summariseCodeUse(list(cs = 123),
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = 25))
  expect_error(summariseCodeUse(list(cs = 123),
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = list(c(18,17))))
  expect_error(summariseCodeUse(list(cs = 123),
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = list(c(0,17),
                                                c(15,20))))


  CDMConnector::cdmDisconnect(cdm)


})

test_that("snowflake", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()

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
