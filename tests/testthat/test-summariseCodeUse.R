test_that("summarise code use", {

  testthat::skip_if(Sys.getenv("CDM5_REDSHIFT_DBNAME") == "")

  db <-  DBI::dbConnect(RPostgres::Redshift(),
                 dbname   = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
                 host     = Sys.getenv("CDM5_REDSHIFT_HOST"),
                 port     = Sys.getenv("CDM5_REDSHIFT_PORT"),
                 user     = Sys.getenv("CDM5_REDSHIFT_USER"),
                 password = Sys.getenv("CDM5_REDSHIFT_PASSWORD"))

  cdm <- CDMConnector::cdm_from_con(con = db,
                                    cdm_schema = Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA"))

  asthma <- c(317009, 257581)

  results <- summariseCodeUse(asthma,
                              cdm = cdm,
                              byYear = TRUE,
                              bySex = TRUE,
                              ageGroup = list(c(0,17),
                                              c(18,65),
                                              c(66, 100)))
  # column names
  expect_true(tibble::is_tibble(results))

  expect_true(all(colnames(results) %in%
                    c("group_name", "group_level",
                      "strata_name", "strata_level",
                      "variable_name", "variable_level",
                      "variable_type",
                      "estimate_type", "estimate",
                      "estimate_suppressed")
                ))


  # overall record count
  expect_true(results %>%
    dplyr::filter(group_name == "Codelist" &
                  strata_name == "Overall" &
                  strata_level == "Overall",
                  variable_name == "Record count") %>%
    dplyr::pull("estimate") ==
  cdm$condition_occurrence %>%
    dplyr::filter(condition_concept_id %in%  asthma) %>%
    dplyr::tally() %>%
    dplyr::pull("n"))

  # overall person count
  expect_true(results %>%
                dplyr::filter(group_name == "Codelist" &
                                strata_name == "Overall" &
                                strata_level == "Overall" &
                              variable_name == "Person count") %>%
                dplyr::pull("estimate") ==
                cdm$condition_occurrence %>%
                dplyr::filter(condition_concept_id %in% asthma) %>%
                dplyr::select("person_id") %>%
                dplyr::distinct() %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # by year
  # overall record count
  expect_true(results %>%
                dplyr::filter(group_name == "Codelist" &
                                strata_name == "Year" &
                                strata_level == "2008",
                              variable_name == "Record count") %>%
                dplyr::pull("estimate") ==
                cdm$condition_occurrence %>%
                dplyr::filter(condition_concept_id %in% asthma) %>%
                dplyr::filter(year(condition_start_date) == 2008) %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # overall person count
  expect_true(results %>%
                dplyr::filter(group_name == "Codelist" &
                                strata_name == "Year" &
                                strata_level == "2008",
                              variable_name == "Person count") %>%
                dplyr::pull("estimate") ==
                cdm$condition_occurrence %>%
                dplyr::filter(condition_concept_id %in% asthma) %>%
                dplyr::filter(year(condition_start_date) == 2008) %>%
                dplyr::select("person_id") %>%
                dplyr::distinct() %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # by age group and sex
  # overall record count
  expect_true(results %>%
                dplyr::filter(group_name == "Codelist" &
                                strata_name == "Sex" &
                                strata_level == "Male",
                              variable_name == "Record count") %>%
                dplyr::pull("estimate") ==
                cdm$condition_occurrence %>%
                dplyr::filter(condition_concept_id %in% asthma) %>%
                PatientProfiles::addSex(cdm) %>%
                dplyr::filter(sex == "Male") %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  expect_true(results %>%
                dplyr::filter(group_name == "Codelist" &
                                strata_name == "Age group and sex" &
                                strata_level == "18 to 65 and Male",
                              variable_name == "Record count") %>%
                dplyr::pull("estimate") ==
                cdm$condition_occurrence %>%
                dplyr::filter(condition_concept_id %in% asthma) %>%
                PatientProfiles::addAge(cdm,
                                        indexDate = "condition_start_date") %>%
                PatientProfiles::addSex(cdm) %>%
                dplyr::filter(sex == "Male" &
                              age >= "18" &
                              age <= "65") %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # overall person count
  expect_true(results %>%
                dplyr::filter(group_name == "Codelist" &
                                strata_name == "Age group and sex" &
                                strata_level == "18 to 65 and Male",
                              variable_name == "Person count") %>%
                dplyr::pull("estimate") ==
                cdm$condition_occurrence %>%
                dplyr::filter(condition_concept_id %in% asthma) %>%
                PatientProfiles::addAge(cdm,
                                        indexDate = "condition_start_date") %>%
                PatientProfiles::addSex(cdm) %>%
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
    dplyr::filter(variable_name == "Person count")) > 0)
  expect_true(nrow(results %>%
    dplyr::filter(variable_name == "Record count")) == 0)

  results <- summariseCodeUse(asthma,
                              cdm = cdm, countBy = "record",
                              byYear = FALSE,
                              bySex = FALSE,
                              ageGroup = NULL)
  expect_true(nrow(results %>%
                     dplyr::filter(variable_name == "Person count")) == 0)
  expect_true(nrow(results %>%
                     dplyr::filter(variable_name == "Record count")) > 0)

  # check min cell count
  results <- summariseCodeUse(asthma,
                              cdm = cdm,
                              byYear = FALSE,
                              bySex = FALSE,
                              ageGroup = NULL,
                              minCellCount = 75)
  expect_true(max(results$estimate, na.rm = TRUE) >=75)

# domains covered

  # condition
  expect_true(nrow(summariseCodeUse(c(317009),
                              cdm = cdm,
                              byYear = FALSE,
                              bySex = FALSE,
                              ageGroup = NULL))>1)

  # visit
  expect_true(nrow(summariseCodeUse(9201,
                              cdm = cdm,
                              byYear = FALSE,
                              bySex = FALSE,
                              ageGroup = NULL))>1)

# drug
expect_true(nrow(summariseCodeUse(19071493,
                             cdm = cdm,
                            byYear = FALSE,
                            bySex = FALSE,
                            ageGroup = NULL))>1)

# measurement
expect_true(nrow(summariseCodeUse(2212542,
                            cdm = cdm,
                            byYear = FALSE,
                            bySex = FALSE,
                            ageGroup = NULL))>1)

# procedure and condition
expect_true(nrow(summariseCodeUse(c(4261206,317009),
                            cdm = cdm,
                            byYear = FALSE,
                            bySex = FALSE,
                            ageGroup = NULL))>1)

# no records
expect_message(results <- summariseCodeUse(c(999999),
                 cdm = cdm,
                 byYear = FALSE,
                 bySex = FALSE,
                 ageGroup = NULL))
expect_true(nrow(results) == 0)



# expected errors
  expect_error(summariseCodeUse("not a concept",
                   cdm = cdm,
                   byYear = FALSE,
                   bySex = FALSE,
                   ageGroup = NULL))
  expect_error(summariseCodeUse(123,
                   cdm = "not a cdm",
                   byYear = FALSE,
                   bySex = FALSE,
                   ageGroup = NULL))
  expect_error(summariseCodeUse(123,
                   cdm = cdm,
                   byYear = "Maybe",
                   bySex = FALSE,
                   ageGroup = NULL))
  expect_error(summariseCodeUse(123,
                   cdm = cdm,
                   byYear = FALSE,
                   bySex = "Maybe",
                   ageGroup = NULL))
  expect_error(summariseCodeUse(123,
                   cdm = cdm,
                   byYear = FALSE,
                   bySex = FALSE,
                   ageGroup = 25))
  expect_error(summariseCodeUse(123,
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = list(c(18,17))))
  expect_error(summariseCodeUse(123,
                  cdm = cdm,
                  byYear = FALSE,
                  bySex = FALSE,
                  ageGroup = list(c(0,17),
                                  c(15,20))))


  CDMConnector::cdmDisconnect(cdm)


})
