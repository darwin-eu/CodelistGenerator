test_that("summarise code use - eunomia", {
skip_on_cran()
  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
  }
  if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) {
    dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
  }
  if (!CDMConnector::eunomia_is_available()) {
    invisible(utils::capture.output(CDMConnector::downloadEunomiaData(pathToData = Sys.getenv("EUNOMIA_DATA_FOLDER"))))
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(con, cdm_schem = "main", write_schema = "main")

  acetiminophen <- c(1125315,  1127433, 40229134,
                    40231925, 40162522, 19133768,  1127078)
  poliovirus_vaccine <- c(40213160)
  cs <- list(acetiminophen = acetiminophen,
             poliovirus_vaccine = poliovirus_vaccine)

  results <- summariseCodeUse(cs,
                              cdm = cdm,
                              byYear = TRUE,
                              bySex = TRUE,
                              ageGroup = list(c(0,17),
                                              c(18,65),
                                              c(66, 100)))
  # column names
  expect_true(tibble::is_tibble(results))

  expect_equal(colnames(results),
                    c("group_name", "group_level",
                      "strata_name", "strata_level",
                      "variable_name", "variable_level",
                      "variable_type",
                      "estimate_type", "estimate",
                      "estimate_suppressed",
                      "standard_concept_name",
                      "standard_concept_id",
                      "source_concept_name",
                      "source_concept_id",
                      "domain_id",
                      "codelist_name",
                      "cohort_name"))


  # overall record count
  expect_true(results %>%
                dplyr::filter(group_name == "Codelist" &
                                strata_name == "Overall" &
                                strata_level == "Overall" &
                                codelist_name == "acetiminophen" &
                              variable_name == "Record count") %>%
                dplyr::pull("estimate") ==
                cdm$drug_exposure %>%
                dplyr::filter(drug_concept_id %in%  acetiminophen) %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # overall person count
  expect_true(results %>%
                dplyr::filter(group_name == "Codelist" &
                                strata_name == "Overall" &
                                strata_level == "Overall" &
                                codelist_name == "acetiminophen" &
                                variable_name == "Person count") %>%
                dplyr::pull("estimate") ==
                cdm$drug_exposure %>%
                dplyr::filter(drug_concept_id %in% acetiminophen) %>%
                dplyr::select("person_id") %>%
                dplyr::distinct() %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # by year
  # overall record count
  expect_true(results %>%
                dplyr::filter(group_name == "Codelist" &
                                strata_name == "Year" &
                                strata_level == "2008" &
                              codelist_name == "acetiminophen" &
                              variable_name == "Record count") %>%
                dplyr::pull("estimate") ==
                cdm$drug_exposure %>%
                dplyr::filter(drug_concept_id %in% acetiminophen) %>%
                dplyr::filter(year(drug_exposure_start_date) == 2008) %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # overall person count
  expect_true(results %>%
                dplyr::filter(group_name == "Codelist" &
                                strata_name == "Year" &
                                strata_level == "2008" &
                                codelist_name == "acetiminophen" &
                              variable_name == "Person count") %>%
                dplyr::pull("estimate") ==
                cdm$drug_exposure %>%
                dplyr::filter(drug_concept_id %in% acetiminophen) %>%
                dplyr::filter(year(drug_exposure_start_date) == 2008) %>%
                dplyr::select("person_id") %>%
                dplyr::distinct() %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  # by age group and sex
  # overall record count
  expect_true(results %>%
                dplyr::filter(group_name == "Codelist" &
                                strata_name == "Sex" &
                                strata_level == "Male" &
                                codelist_name == "acetiminophen" &
                              variable_name == "Record count") %>%
                dplyr::pull("estimate") ==
                cdm$drug_exposure %>%
                dplyr::filter(drug_concept_id %in% acetiminophen) %>%
                PatientProfiles::addSex(cdm) %>%
                dplyr::filter(sex == "Male") %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  expect_true(results %>%
                dplyr::filter(group_name == "Codelist" &
                                strata_name == "Age group and sex" &
                                strata_level == "18 to 65 and Male" &
                                codelist_name == "acetiminophen" &
                              variable_name == "Record count") %>%
                dplyr::pull("estimate") ==
                cdm$drug_exposure %>%
                dplyr::filter(drug_concept_id %in% acetiminophen) %>%
                PatientProfiles::addAge(cdm,
                                        indexDate = "drug_exposure_start_date") %>%
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
                                strata_level == "18 to 65 and Male" &
                                codelist_name == "acetiminophen" &
                              variable_name == "Person count") %>%
                dplyr::pull("estimate") ==
                cdm$drug_exposure %>%
                dplyr::filter(drug_concept_id %in% acetiminophen) %>%
                PatientProfiles::addAge(cdm,
                                        indexDate = "drug_exposure_start_date") %>%
                PatientProfiles::addSex(cdm) %>%
                dplyr::filter(sex == "Male" &
                                age >= "18" &
                                age <= "65") %>%
                dplyr::select("person_id") %>%
                dplyr::distinct() %>%
                dplyr::tally() %>%
                dplyr::pull("n"))

  results <- summariseCodeUse(list("acetiminophen" = acetiminophen),
                              cdm = cdm, countBy = "person",
                              byYear = FALSE,
                              bySex = FALSE,
                              ageGroup = NULL)
  expect_true(nrow(results %>%
                     dplyr::filter(variable_name == "Person count")) > 0)
  expect_true(nrow(results %>%
                     dplyr::filter(variable_name == "Record count")) == 0)

  results <- summariseCodeUse(list("acetiminophen" = acetiminophen),
                              cdm = cdm, countBy = "record",
                              byYear = FALSE,
                              bySex = FALSE,
                              ageGroup = NULL)
  expect_true(nrow(results %>%
                     dplyr::filter(variable_name == "Person count")) == 0)
  expect_true(nrow(results %>%
                     dplyr::filter(variable_name == "Record count")) > 0)

  # check min cell count
  results <- summariseCodeUse(list("acetiminophen" = acetiminophen),
                              cdm = cdm,
                              byYear = FALSE,
                              bySex = FALSE,
                              ageGroup = NULL,
                              minCellCount = 75)
  expect_true(max(results$estimate, na.rm = TRUE) >=75)

  # domains covered
  # condition
  expect_true(nrow(summariseCodeUse(list(cs= c(4112343)),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # visit
  expect_true(nrow(summariseCodeUse(list(cs= c(9201)),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # drug
  expect_true(nrow(summariseCodeUse(list(cs= c(40213160)),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # measurement
  expect_true(nrow(summariseCodeUse(list(cs= c(3006322)),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # procedure and condition
  expect_true(nrow(summariseCodeUse(list(cs= c(4107731,4112343)),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # no records
  expect_message(results <- summariseCodeUse(list(cs= c(999999)),
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
  expect_error(summariseCodeUse("123",
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseCodeUse(list("123"), # not named
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseCodeUse(list(a = 123),
                                cdm = "not a cdm",
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseCodeUse(list(a = 123),
                                cdm = cdm,
                                byYear = "Maybe",
                                bySex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseCodeUse(list(a = 123),
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = "Maybe",
                                ageGroup = NULL))
  expect_error(summariseCodeUse(list(a = 123),
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = 25))
  expect_error(summariseCodeUse(list(a = 123),
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = list(c(18,17))))
  expect_error(summariseCodeUse(list(a = 123),
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = list(c(0,17),
                                                c(15,20))))

  CDMConnector::cdmDisconnect(cdm)


})

test_that("summarise cohort code use - eunomia", {
  skip_on_cran()
  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
  }
  if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) {
    dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
  }
  if (!CDMConnector::eunomia_is_available()) {
    invisible(utils::capture.output(CDMConnector::downloadEunomiaData(pathToData = Sys.getenv("EUNOMIA_DATA_FOLDER"))))
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(con, cdm_schem = "main", write_schema = "main")

  pharyngitis <- c(4112343)

  cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                                conceptSet = list(pharyngitis = pharyngitis),
                                                name = "pharyngitis",
                                                end = "observation_period_end_date",
                                                overwrite = TRUE)

  # any
  results_all <- summariseCodeUse(list(cs = 4134304),
                                  cdm = cdm, minCellCount = 0)
  results_cohort <- summariseCohortCodeUse(list(cs = 4134304),
                                           cdm = cdm,
                                           cohortTable = "pharyngitis",
                                           timing = "any", minCellCount = 0)

  expect_true(tibble::is_tibble(results_cohort))
  expect_true(all(colnames(results_cohort) %in%
                    c("group_name", "group_level",
                      "strata_name", "strata_level",
                      "variable_name", "variable_level",
                      "variable_type",
                      "estimate_type", "estimate",
                      "estimate_suppressed",
                      "standard_concept_name",
                      "standard_concept_id",
                      "source_concept_name",
                      "source_concept_id",
                      "domain_id",
                      "codelist_name",
                      "cohort_name")
  ))


  expect_true(results_cohort %>%
                dplyr::filter(group_name == "Codelist" &
                                strata_name == "Overall" &
                                strata_level == "Overall" &
                                variable_name == "Person count") %>%
                dplyr::pull("estimate") <
                results_all %>%
                dplyr::filter(group_name == "Codelist" &
                                strata_name == "Overall" &
                                strata_level == "Overall" &
                                variable_name == "Person count") %>%
                dplyr::pull("estimate"))




  # at entry - everyone in the cohort should have the code
  results_cohort <- summariseCohortCodeUse(list(pharyngitis = pharyngitis),
                                           cdm = cdm,
                                           cohortTable = "pharyngitis",
                                           timing = "entry",
                                           minCellCount = 0)
  results_cohort %>%
    dplyr::filter(group_name == "Codelist" &
                    strata_name == "Overall" &
                    strata_level == "Overall" &
                    variable_name == "Person count") %>%
    dplyr::pull("estimate") ==
    CDMConnector::cohortCount(cdm$pharyngitis) %>%
    dplyr::pull("number_subjects")



  # 260139
  # on index
  index_260139 <- cdm$pharyngitis %>%
    dplyr::left_join(cdm$condition_occurrence,
                     by=c("subject_id"="person_id")) %>%
    dplyr::filter(condition_start_date == cohort_start_date) %>%
    dplyr::filter(condition_concept_id == 260139) %>%
    dplyr::select("subject_id") %>%
    dplyr::distinct() %>%
    dplyr::count() %>%
    dplyr::pull()

  results_cohort_260139 <- summariseCohortCodeUse(list(cs = 260139),
                                                  cdm = cdm,
                                                  cohortTable = "pharyngitis",
                                                  timing = "entry",
                                                  minCellCount = 0)
  expect_equal(results_cohort_260139 %>%
                 dplyr::filter(group_name == "Codelist" &
                                 strata_name == "Overall" &
                                 strata_level == "Overall" &
                                 variable_name == "Person count") %>%
                 dplyr::pull("estimate"), index_260139)


  # 260139 or 19133873 or 1127433
  # on index
  index_260139_19133873_1127433 <- dplyr::union_all(
    cdm$pharyngitis %>%
      dplyr::left_join(cdm$condition_occurrence,
                       by=c("subject_id"="person_id")) %>%
      dplyr::filter(condition_start_date == cohort_start_date) %>%
      dplyr::filter(condition_concept_id == 260139) %>%
      dplyr::select("subject_id"),
    cdm$pharyngitis %>%
      dplyr::left_join(cdm$drug_exposure,
                       by=c("subject_id"="person_id")) %>%
      dplyr::filter(drug_exposure_start_date == cohort_start_date) %>%
      dplyr::filter(drug_concept_id %in% c(19133873,1127433)) %>%
      dplyr::select("subject_id")) %>%
    dplyr::count() %>%
    dplyr::pull()

  results_cohort_260139_19133873_1127433<- summariseCohortCodeUse(list(cs = c(260139,19133873,1127433)),
                                                                  cdm = cdm,
                                                                  cohortTable = "pharyngitis",
                                                                  timing = "entry",
                                                                  minCellCount = 0)
  expect_equal(results_cohort_260139_19133873_1127433 %>%
                 dplyr::filter(group_name == "Codelist" &
                                 strata_name == "Overall" &
                                 strata_level == "Overall" &
                                 variable_name == "Record count") %>%
                 dplyr::pull("estimate"),
               index_260139_19133873_1127433)

  expect_equal(results_cohort_260139_19133873_1127433 %>%
                 dplyr::filter(stringr::str_detect(group_level, "Acute bronchitis")) %>%
                 dplyr::filter(strata_name == "Overall" &
                                 strata_level == "Overall" &
                                 variable_name == "Person count") %>%
                 dplyr::pull("estimate"),
               index_260139)


  # multiple cohorts
  cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                                conceptSet = list(a = 260139,
                                                                  b = 1127433 ),
                                                name = "cohorts",
                                                end = "observation_period_end_date",
                                                overwrite = TRUE)

  results_cohort_mult <- summariseCohortCodeUse(list(cs = c(260139,19133873,1127433)),
                                                                  cdm = cdm,
                                                                  cohortTable = "cohorts",
                                                                  timing = "entry",
                                                                 minCellCount = 0)
  expect_true(nrow(results_cohort_mult %>%
                     dplyr::filter(stringr::str_detect(group_level, "Acute bronchitis")) %>%
    dplyr::filter(strata_name == "Overall" &
                    strata_level == "Overall" &
                    variable_name == "Person count")) == 2)

  expect_equal(c("a", "b"),  results_cohort_mult %>%
                 dplyr::filter(stringr::str_detect(group_level, "Acute bronchitis")) %>%
   dplyr::filter(strata_name == "Overall" &
                   strata_level == "Overall" &
                   variable_name == "Person count") %>%
   dplyr::pull("cohort_name"))


  # empty cohort - no results
  cdm$pharyngitis <-  cdm$pharyngitis %>%
    dplyr::filter(cohort_definition_id == 99)
  expect_true(nrow(summariseCohortCodeUse(list(cs = 4134304),
                                         cdm = cdm,
                                         cohortTable = "pharyngitis",
                                         timing = "any", minCellCount = 0)) == 0)

  # expected errors
  expect_error(summariseCohortCodeUse(4134304,
                                      cdm = cdm,
                                      cohortTable = "not_a_cohort",
                                      timing = "any"))
  expect_error(summariseCohortCodeUse(list(4134304),
                                      cdm = cdm,
                                      cohortTable = "not_a_cohort",
                                      timing = "any"))
  expect_error(summariseCohortCodeUse(list(cs = 4134304),
                                      cdm = cdm,
                                      cohortTable = "not_a_cohort",
                                      timing = "any"))
  expect_error(summariseCohortCodeUse(list(cs = 4134304),
                                      cdm = cdm,
                                      cohortTable = "pharyngitis",
                                      timing = "not_a_option"))
  expect_error(summariseCohortCodeUse(list(cs = 4134304),
                                      cdm = cdm,
                                      cohortTable = "pharyngitis",
                                      timing = c("any", "entry")))

  CDMConnector::cdmDisconnect(cdm)

})

test_that("summarise code use - redshift", {

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

  asthma <- list(asthma = c(317009, 257581))

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
                      "estimate_suppressed",
                      "standard_concept_name",
                      "standard_concept_id",
                      "source_concept_name",
                      "source_concept_id",
                      "domain_id",
                      "codelist_name",
                      "cohort_name")
                ))


  # overall record count
  expect_true(results %>%
    dplyr::filter(group_name == "Codelist" &
                  strata_name == "Overall" &
                  strata_level == "Overall",
                  variable_name == "Record count") %>%
    dplyr::pull("estimate") ==
  cdm$condition_occurrence %>%
    dplyr::filter(condition_concept_id %in%  !!asthma[[1]]) %>%
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
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) %>%
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
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) %>%
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
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) %>%
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
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) %>%
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
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) %>%
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
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) %>%
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




