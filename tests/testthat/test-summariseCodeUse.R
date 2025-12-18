test_that("summarise code use - eunomia", {
  skip_on_cran()

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main", writeSchema = "main")

  acetiminophen <- c(1125315L,  1127433L, 40229134L,
                     40231925L, 40162522L, 19133768L,  1127078L)
  poliovirus_vaccine <- c(40213160L)
  cs <- omopgenerics::newCodelist(list(acetiminophen = acetiminophen,
                                       poliovirus_vaccine = poliovirus_vaccine))
  startNames <- omopgenerics::listSourceTables(cdm)

  results <- summariseCodeUse(cs,
                              cdm = cdm,
                              byYear = TRUE,
                              bySex = TRUE,
                              ageGroup = list(c(0,17),
                                              c(18,65),
                                              c(66, 100)))
  endNames <- omopgenerics::listSourceTables(cdm)
  expect_true(length(setdiff(endNames, startNames)) == 0)

  expect_no_error(results_no_by_concept <- summariseCodeUse(cs,
                                                            cdm = cdm,
                                                            byYear = TRUE,
                                                            bySex = TRUE,
                                                            byConcept = FALSE))
  expect_true(all(results_no_by_concept |> dplyr::pull("group_level") |> unique() == c("acetiminophen","poliovirus_vaccine")))
  expect_true(all(results_no_by_concept |>
                    dplyr::filter(group_level == "acetiminophen", strata_level == "overall") |>
                    dplyr::pull("estimate_value") == c("14205", "2679")))

  # min cell counts:
  expect_true(
    all(
      omopgenerics::suppress(results) |>
        dplyr::filter(
          variable_name == "overall",
          strata_level == "1909",
          group_level == "acetiminophen"
        ) |>
        dplyr::pull("estimate_value") == "-"
    ))

  # check is a summarised result
  expect_true("summarised_result" %in%  class(results))
  expect_equal(omopgenerics::resultColumns(),
               colnames(results))

  # overall record count
  expect_true(results |>
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "overall" &
                                strata_level == "overall" &
                                group_level == "acetiminophen" &
                                estimate_name == "record_count",
                              variable_name == "overall") |>
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$drug_exposure |>
                dplyr::filter(drug_concept_id %in%  acetiminophen) |>
                dplyr::tally() |>
                dplyr::pull("n"))

  # overall person count
  expect_true(results |>
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "overall" &
                                strata_level == "overall" &
                                group_level == "acetiminophen" &
                                estimate_name == "person_count",
                              variable_name == "overall") |>
                dplyr::pull("estimate_value") |>
                as.numeric()  ==
                cdm$drug_exposure |>
                dplyr::filter(drug_concept_id %in% acetiminophen) |>
                dplyr::select("person_id") |>
                dplyr::distinct() |>
                dplyr::tally() |>
                dplyr::pull("n"))

  # by year
  # overall record count
  expect_true(results |>
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "year" &
                                strata_level == "2008" &
                                group_level == "acetiminophen" &
                                estimate_name == "record_count",
                              variable_name == "overall") |>
                dplyr::pull("estimate_value") |>
                as.numeric()  ==
                cdm$drug_exposure |>
                dplyr::filter(drug_concept_id %in% acetiminophen) |>
                dplyr::filter(year(drug_exposure_start_date) == 2008) |>
                dplyr::tally() |>
                dplyr::pull("n"))

  # overall person count
  expect_true(results |>
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "year" &
                                strata_level == "2008" &
                                group_level == "acetiminophen" &
                                estimate_name == "person_count",
                              variable_name == "overall") |>
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$drug_exposure |>
                dplyr::filter(drug_concept_id %in% acetiminophen) |>
                dplyr::filter(year(drug_exposure_start_date) == 2008) |>
                dplyr::select("person_id") |>
                dplyr::distinct() |>
                dplyr::tally() |>
                dplyr::pull("n"))

  # by age group and sex
  # overall record count
  expect_true(results |>
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "sex" &
                                strata_level == "Male" &
                                group_level == "acetiminophen" &
                                estimate_name == "record_count",
                              variable_name == "overall") |>
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$drug_exposure |>
                dplyr::filter(drug_concept_id %in% acetiminophen) |>
                PatientProfiles::addSex() |>
                dplyr::filter(sex == "Male") |>
                dplyr::tally() |>
                dplyr::pull("n"))

  expect_true(results |>
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "age_group &&& sex" &
                                strata_level == "18 to 65 &&& Male" &
                                group_level == "acetiminophen" &
                                estimate_name == "record_count",
                              variable_name == "overall") |>
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$drug_exposure |>
                dplyr::filter(drug_concept_id %in% acetiminophen) |>
                PatientProfiles::addAge(indexDate = "drug_exposure_start_date") |>
                PatientProfiles::addSex() |>
                dplyr::filter(sex == "Male" &
                                age >= "18" &
                                age <= "65") |>
                dplyr::tally() |>
                dplyr::pull("n"))

  # overall person count
  expect_true(results |>
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "age_group &&& sex" &
                                strata_level == "18 to 65 &&& Male" &
                                group_level == "acetiminophen" &
                                estimate_name == "person_count",
                              variable_name == "overall") |>
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$drug_exposure |>
                dplyr::filter(drug_concept_id %in% acetiminophen) |>
                PatientProfiles::addAge(indexDate = "drug_exposure_start_date") |>
                PatientProfiles::addSex() |>
                dplyr::filter(sex == "Male" &
                                age >= "18" &
                                age <= "65") |>
                dplyr::select("person_id") |>
                dplyr::distinct() |>
                dplyr::tally() |>
                dplyr::pull("n"))

  # Check date range
  results <- summariseCodeUse(cs,
                              cdm = cdm,
                              byYear = TRUE,
                              bySex = TRUE,
                              ageGroup = list(c(0,17),
                                              c(18,65),
                                              c(66, 100)),
                              dateRange = as.Date(c("2010-01-01","2015-01-01")))
  expect_equal(results |>
                 omopgenerics::settings() |>
                 dplyr::select("date_range_start", "date_range_end") |>
                 as.character(),
               c("2010-01-01","2015-01-01"))

  # overall record count
  expect_true(results |>
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "overall" &
                                strata_level == "overall" &
                                group_level == "acetiminophen" &
                                estimate_name == "record_count",
                              variable_name == "overall") |>
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$drug_exposure |>
                dplyr::filter(drug_exposure_start_date >= as.Date("2010-01-01"),
                              drug_exposure_start_date <= as.Date("2015-01-01")) |>
                dplyr::filter(drug_concept_id %in%  acetiminophen) |>
                dplyr::tally() |>
                dplyr::pull("n"))

  # overall person count
  expect_true(results |>
                dplyr::filter(group_name == "codelist_name" &
                                strata_name == "overall" &
                                strata_level == "overall" &
                                group_level == "acetiminophen" &
                                estimate_name == "person_count",
                              variable_name == "overall") |>
                dplyr::pull("estimate_value") |>
                as.numeric()  ==
                cdm$drug_exposure |>
                dplyr::filter(drug_concept_id %in% acetiminophen) |>
                dplyr::filter(drug_exposure_start_date >= as.Date("2010-01-01"),
                              drug_exposure_start_date <= as.Date("2014-12-31")) |>
                dplyr::select("person_id") |>
                dplyr::distinct() |>
                dplyr::tally() |>
                dplyr::pull("n"))

  # by year
  expect_true(
    results |>
      dplyr::filter(strata_name == "year") |>
      dplyr::pull("strata_level") |>
      unique() |>
      as.numeric() |> max() == 2014)
  expect_true(
    results |>
      dplyr::filter(strata_name == "year") |>
      dplyr::pull("strata_level") |>
      unique() |>
      as.numeric() |> min() == 2010)



  results <- summariseCodeUse(omopgenerics::newCodelist(list("acetiminophen" = acetiminophen)),
                              cdm = cdm, countBy = "person",
                              byYear = FALSE,
                              bySex = FALSE,
                              ageGroup = NULL)
  expect_true(nrow(results |>
                     dplyr::filter(estimate_name == "person_count")) > 0)
  expect_true(nrow(results |>
                     dplyr::filter(estimate_name == "record_count")) == 0)

  results <- summariseCodeUse(omopgenerics::newCodelist(list("acetiminophen" = acetiminophen)),
                              cdm = cdm, countBy = "record",
                              byYear = FALSE,
                              bySex = FALSE,
                              ageGroup = NULL)
  expect_true(nrow(results |>
                     dplyr::filter(estimate_name == "person_count")) == 0)
  expect_true(nrow(results |>
                     dplyr::filter(estimate_name == "record_count")) > 0)

  # domains covered
  # condition
  expect_true(nrow(summariseCodeUse(omopgenerics::newCodelist(list(cs= c(4112343L))),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # visit
  expect_true(nrow(summariseCodeUse(omopgenerics::newCodelist(list(cs= c(9201L))),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # drug
  expect_true(nrow(summariseCodeUse(omopgenerics::newCodelist(list(cs= c(40213160L))),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # measurement
  expect_true(nrow(summariseCodeUse(omopgenerics::newCodelist(list(cs= c(3006322L))),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # procedure and condition
  expect_true(nrow(summariseCodeUse(omopgenerics::newCodelist(list(cs= c(4107731L,4112343L))),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # no records
  expect_message(results <- summariseCodeUse(omopgenerics::newCodelist(list(cs= c(999999L))),
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
  expect_error(summariseCodeUse(list("123" = 1L),
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseCodeUse(omopgenerics::newCodelist(list(a = 123L)),
                                cdm = "not a cdm",
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseCodeUse(omopgenerics::newCodelist(list(a = 123L)),
                                cdm = cdm,
                                byYear = "Maybe",
                                bySex = FALSE,
                                ageGroup = NULL))
  expect_error(summariseCodeUse(omopgenerics::newCodelist(list(a = 123L)),
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = "Maybe",
                                ageGroup = NULL))
  expect_error(summariseCodeUse(omopgenerics::newCodelist(list(a = 123L)),
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = 25))
  expect_error(summariseCodeUse(omopgenerics::newCodelist(list(a = 123L)),
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = list(c(18,17))))
  expect_error(summariseCodeUse(omopgenerics::newCodelist(list(a = 123L)),
                                cdm = cdm,
                                byYear = FALSE,
                                bySex = FALSE,
                                ageGroup = list(c(0,17),
                                                c(15,20))))
  expect_error(summariseCodeUse(omopgenerics::newCodelist(list(a = 123L)),
                                cdm = cdm,
                                dateRange = c("a","b")))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("summarise cohort code use - eunomia", {
  skip_on_cran()

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(cdmName = "cdm", con, cdmSchema = "main", writeSchema = "main")

  pharyngitis_codes <- omopgenerics::newCodelist(list("ph" = c(4112343L)))

  cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                                conceptSet = pharyngitis_codes,
                                                name = "pharyngitis",
                                                end = "observation_period_end_date",
                                                overwrite = TRUE)

  # any
  results_all <- summariseCodeUse(pharyngitis_codes,
                                  cdm = cdm)
  results_cohort <- summariseCohortCodeUse(x = pharyngitis_codes,
                                           cdm = cdm,
                                           cohortTable = "pharyngitis",
                                           cohortId = "ph",
                                           timing = "any")
  expect_identical(results_cohort,
                   results_cohort)

  results_cohort_attr <- summariseCohortCodeUse(cdm = cdm,
                                                cohortTable = "pharyngitis",
                                                timing = "any")
  expect_identical(results_cohort, results_cohort_attr)
  expect_no_error(summariseCohortCodeUse(x = omopgenerics::newCodelist(list(cs = 4134304L)),
                                         cdm = cdm,
                                         cohortTable = "pharyngitis",
                                         timing = "any",
                                         byConcept = FALSE))
  expect_true(inherits(results_cohort, "summarised_result"))
  expect_true(all(c("result_id", "result_type", "package_name", "package_version", "timing") %in%
                    colnames(omopgenerics::settings(results_cohort))))

  expect_true(results_cohort |>
                dplyr::filter(variable_name == "overall" &
                                strata_name == "overall" &
                                strata_level == "overall" &
                                estimate_name == "person_count") |>
                dplyr::pull("estimate_value") |>
                as.numeric() <=
                results_all |>
                dplyr::filter(variable_name == "overall" &
                                strata_name == "overall" &
                                strata_level == "overall" &
                                estimate_name == "person_count") |>
                dplyr::pull("estimate_value") |>
                as.numeric())




  # at entry - everyone in the cohort should have the code
  results_cohort <- summariseCohortCodeUse(pharyngitis_codes,
                                           cdm = cdm,
                                           cohortTable = "pharyngitis",
                                           timing = "entry")
  results_cohort |>
    dplyr::filter(variable_name == "overall" &
                    strata_name == "overall" &
                    strata_level == "overall" &
                    estimate_name == "person_count") |>
    dplyr::pull("estimate_value") |>
    as.numeric() ==
    CDMConnector::cohortCount(cdm$pharyngitis) |>
    dplyr::pull("number_subjects")



  # 260139
  # on index
  index_260139 <- cdm$pharyngitis |>
    dplyr::left_join(cdm$condition_occurrence,
                     by=c("subject_id"="person_id")) |>
    dplyr::filter(condition_start_date == cohort_start_date) |>
    dplyr::filter(condition_concept_id == 260139) |>
    dplyr::select("subject_id") |>
    dplyr::distinct() |>
    dplyr::count() |>
    dplyr::pull()

  results_cohort_260139 <- summariseCohortCodeUse(omopgenerics::newCodelist(list(cs = 260139L)),
                                                  cdm = cdm,
                                                  cohortTable = "pharyngitis",
                                                  timing = "entry")
  expect_equal(results_cohort_260139 |>
                 dplyr::filter(variable_name == "overall" &
                                 strata_name == "overall" &
                                 strata_level == "overall" &
                                 estimate_name == "person_count") |>
                 dplyr::pull("estimate_value") |>
                 as.numeric(), index_260139)


  # 260139 or 19133873 or 1127433
  # on index
  index_260139_19133873_1127433 <- dplyr::union_all(
    cdm$pharyngitis |>
      dplyr::left_join(cdm$condition_occurrence,
                       by=c("subject_id"="person_id")) |>
      dplyr::filter(condition_start_date == cohort_start_date) |>
      dplyr::filter(condition_concept_id == 260139) |>
      dplyr::select("subject_id"),
    cdm$pharyngitis |>
      dplyr::left_join(cdm$drug_exposure,
                       by=c("subject_id"="person_id")) |>
      dplyr::filter(drug_exposure_start_date == cohort_start_date) |>
      dplyr::filter(drug_concept_id %in% c(19133873,1127433)) |>
      dplyr::select("subject_id")) |>
    dplyr::count() |>
    dplyr::pull()

  results_cohort_260139_19133873_1127433<- summariseCohortCodeUse(omopgenerics::newCodelist(list(cs = c(260139L,19133873L,1127433L))),
                                                                  cdm = cdm,
                                                                  cohortTable = "pharyngitis",
                                                                  timing = "entry")
  expect_equal(results_cohort_260139_19133873_1127433 |>
                 dplyr::filter(variable_name == "overall" &
                                 strata_name == "overall" &
                                 strata_level == "overall" &
                                 estimate_name == "record_count") |>
                 dplyr::pull("estimate_value") |>
                 as.numeric(),
               index_260139_19133873_1127433)

  expect_equal(results_cohort_260139_19133873_1127433 |>
                 dplyr::filter(stringr::str_detect(variable_name, "Acute bronchitis")) |>
                 dplyr::filter(strata_name == "overall" &
                                 strata_level == "overall" &
                                 estimate_name == "person_count") |>
                 dplyr::pull("estimate_value") |>
                 as.numeric(),
               index_260139)


  # multiple cohorts
  cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                                conceptSet = omopgenerics::newCodelist(list(a = 260139L,
                                                                                            b = 1127433L )),
                                                name = "cohorts",
                                                end = "observation_period_end_date",
                                                overwrite = TRUE)

  results_cohort_mult <- summariseCohortCodeUse(omopgenerics::newCodelist(list(cs = c(260139L,19133873L,1127433L))),
                                                cdm = cdm,
                                                cohortTable = "cohorts",
                                                timing = "entry")
  expect_true(nrow(results_cohort_mult |>
                     dplyr::filter(stringr::str_detect(variable_name, "Acute bronchitis")) |>
                     dplyr::filter(strata_name == "overall" &
                                     strata_level == "overall" &
                                     estimate_name == "person_count")) == 2)

  expect_equal(c("a", "b"),  results_cohort_mult |>
                 dplyr::filter(stringr::str_detect(variable_name, "Acute bronchitis")) |>
                 dplyr::filter(strata_name == "overall" &
                                 strata_level == "overall" &
                                 estimate_name == "person_count") |>
                 visOmopResults::splitGroup() |>
                 dplyr::pull("cohort_name"))



  codes<- omopgenerics::newCodelist(list(a = 260139L,
                                         b = 1127433L))
  cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                                conceptSet = codes,
                                                name = "cohorts",
                                                end = "observation_period_end_date",
                                                overwrite = TRUE)
  # if we pass codes we'll end up with 4 results - both codelists for both cohorts
  results_codes <- summariseCohortCodeUse(x = codes,
                                          cdm = cdm,
                                          cohortTable = "cohorts",
                                          timing = "entry")
  expect_true(all(c("a &&& a", "a &&& b", "b &&& a", "b &&& b") %in%
                    (results_codes |>
                       dplyr::pull("group_level") |>
                       unique())))
  # but if use cohort codelist we'll end up with 2 results - codelists for respective cohorts
  results_attr <- summariseCohortCodeUse(cdm = cdm,
                                         cohortTable = "cohorts",
                                         timing = "entry")
  expect_false(all(c("a &&& a", "a &&& b", "b &&& a", "b &&& b") %in%
                     (results_attr |>
                        dplyr::pull("group_level") |>
                        unique())))
  expect_true(all(c("a &&& a",  "b &&& b") %in%
                    (results_attr |>
                       dplyr::pull("group_level") |>
                       unique())))
  expect_identical(
    results_codes |>
      dplyr::filter(group_level == "a &&& a"),
    results_attr |>
      dplyr::filter(group_level == "a &&& a"))
  expect_identical(
    results_codes |>
      dplyr::filter(group_level == "b &&& b"),
    results_attr |>
      dplyr::filter(group_level == "b &&& b"))


  # empty cohort - no results
  cdm$pharyngitis <-  cdm$pharyngitis |>
    dplyr::filter(cohort_definition_id == 99)
  expect_true(nrow(summariseCohortCodeUse(omopgenerics::newCodelist(list(cs = 4134304L)),
                                          cdm = cdm,
                                          cohortTable = "pharyngitis",
                                          timing = "any")) == 0)
  expect_true(nrow(summariseCohortCodeUse(cdm = cdm,
                                          cohortTable = "pharyngitis",
                                          timing = "any")) == 0)

  # expected errors
  expect_error(summariseCohortCodeUse(4134304L,
                                      cdm = cdm,
                                      cohortTable = "not_a_cohort",
                                      timing = "any"))
  expect_error(summariseCohortCodeUse(list(4134304L),
                                      cdm = cdm,
                                      cohortTable = "not_a_cohort",
                                      timing = "any"))
  expect_error(summariseCohortCodeUse(list(cs = 4134304L),
                                      cdm = cdm,
                                      cohortTable = "not_a_cohort",
                                      timing = "any"))
  expect_error(summariseCohortCodeUse(list(cs = 4134304L),
                                      cdm = cdm,
                                      cohortTable = "pharyngitis",
                                      timing = "not_a_option"))
  expect_error(summariseCohortCodeUse(list(cs = 4134304L),
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

  cdm <- CDMConnector::cdmFromCon(cdmName = "cdm",
                                  con = db,
                                  cdmSchema = Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA"),
                                  writeSchema = Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA"),
                                  cdmVersion = "5.3")

  asthma <- list(asthma = c(317009L, 257581L))

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
  expect_true(results |>
                dplyr::filter(variable_name == "overall" &
                                strata_name == "overall" &
                                strata_level == "overall",
                              estimate_name == "record_count") |>
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$condition_occurrence |>
                dplyr::filter(condition_concept_id %in%  !!asthma[[1]]) |>
                dplyr::tally() |>
                dplyr::pull("n"))

  # overall person count
  expect_true(results |>
                dplyr::filter(variable_name == "overall" &
                                strata_name == "overall" &
                                strata_level == "overall" &
                                estimate_name == "person_count") |>
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$condition_occurrence |>
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) |>
                dplyr::select("person_id") |>
                dplyr::distinct() |>
                dplyr::tally() |>
                dplyr::pull("n"))

  # by year
  # overall record count
  expect_true(results |>
                dplyr::filter(variable_name == "overall" &
                                strata_name == "year" &
                                strata_level == "2008",
                              estimate_name == "record_count") |>
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$condition_occurrence |>
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) |>
                dplyr::filter(year(condition_start_date) == 2008) |>
                dplyr::tally() |>
                dplyr::pull("n"))

  # overall person count
  expect_true(results |>
                dplyr::filter(variable_name == "overall" &
                                strata_name == "year" &
                                strata_level == "2008",
                              estimate_name == "person_count") |>
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$condition_occurrence |>
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) |>
                dplyr::filter(year(condition_start_date) == 2008) |>
                dplyr::select("person_id") |>
                dplyr::distinct() |>
                dplyr::tally() |>
                dplyr::pull("n"))

  # by age group and sex
  # overall record count
  expect_true(results |>
                dplyr::filter(variable_name == "overall" &
                                strata_name == "sex" &
                                strata_level == "Male",
                              estimate_name == "record_count") |>
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$condition_occurrence |>
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) |>
                PatientProfiles::addSex() |>
                dplyr::filter(sex == "Male") |>
                dplyr::tally() |>
                dplyr::pull("n"))

  expect_true(results |>
                dplyr::filter(variable_name == "overall" &
                                strata_name == "age_group &&& sex" &
                                strata_level == "18 to 65 &&& Male",
                              estimate_name == "record_count") |>
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$condition_occurrence |>
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) |>
                PatientProfiles::addAge(indexDate = "condition_start_date") |>
                PatientProfiles::addSex() |>
                dplyr::filter(sex == "Male" &
                                age >= "18" &
                                age <= "65") |>
                dplyr::tally() |>
                dplyr::pull("n"))

  # overall person count
  expect_true(results |>
                dplyr::filter(variable_name == "overall" &
                                strata_name == "age_group &&& sex" &
                                strata_level == "18 to 65 &&& Male",
                              estimate_name == "person_count") |>
                dplyr::pull("estimate_value") |>
                as.numeric() ==
                cdm$condition_occurrence |>
                dplyr::filter(condition_concept_id %in% !!asthma[[1]]) |>
                PatientProfiles::addAge(indexDate = "condition_start_date") |>
                PatientProfiles::addSex() |>
                dplyr::filter(sex == "Male" &
                                age >= "18" &
                                age <= "65") |>
                dplyr::select("person_id") |>
                dplyr::distinct() |>
                dplyr::tally() |>
                dplyr::pull("n"))




  results <- summariseCodeUse(asthma,
                              cdm = cdm, countBy = "person",
                              byYear = FALSE,
                              bySex = FALSE,
                              ageGroup = NULL)
  expect_true(nrow(results |>
                     dplyr::filter(estimate_name == "person_count")) > 0)
  expect_true(nrow(results |>
                     dplyr::filter(estimate_name == "record_count")) == 0)

  results <- summariseCodeUse(asthma,
                              cdm = cdm, countBy = "record",
                              byYear = FALSE,
                              bySex = FALSE,
                              ageGroup = NULL)
  expect_true(nrow(results |>
                     dplyr::filter(estimate_name == "person_count")) == 0)
  expect_true(nrow(results |>
                     dplyr::filter(estimate_name == "record_count")) > 0)


  # domains covered

  # condition
  expect_true(nrow(summariseCodeUse(list(cs = c(317009L)),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # visit
  expect_true(nrow(summariseCodeUse(list(cs = 9201L),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # drug
  expect_true(nrow(summariseCodeUse(list(cs = 19071493L),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # measurement
  expect_true(nrow(summariseCodeUse(list(cs = 2212542L),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # procedure and condition
  expect_true(nrow(summariseCodeUse(list(cs = c(4261206L,317009L)),
                                    cdm = cdm,
                                    byYear = FALSE,
                                    bySex = FALSE,
                                    ageGroup = NULL))>1)

  # no records
  expect_message(results <- summariseCodeUse(list(cs = c(999999L)),
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

test_that("summarise code use - eunomia source concept id NA", {
  skip_on_cran()

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main", writeSchema = "main")

  acetiminophen <- c(1125315,  1127433, 40229134,
                     40231925, 40162522, 19133768,  1127078) |>
    as.integer()

  cdm$drug_exposure <- cdm$drug_exposure |>
    dplyr::mutate(drug_source_concept_id = NA_character_)

  cs <- omopgenerics::newCodelist(list(acetiminophen = acetiminophen))
  results <- summariseCodeUse(cs,
                              cdm = cdm)

  expect_true(all(omopgenerics::splitAdditional(results) |>
                    dplyr::filter(variable_name != "overall") |>
                    dplyr::pull("source_concept_name") == "NA"))
  expect_true(all(omopgenerics::splitAdditional(results) |>
                    dplyr::filter(variable_name != "overall") |>
                    dplyr::pull("source_concept_id") == "NA"))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("summarise cohort code use - eunomia source concept id NA", {
  skip_on_cran()

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main", writeSchema = "main")

  pharyngitis <- c(4112343L)

  cdm$condition_occurrence <- cdm$condition_occurrence |>
    dplyr::mutate(condition_source_concept_id = NA_character_)

  cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                                conceptSet = list(pharyngitis = pharyngitis),
                                                name = "pharyngitis",
                                                end = "observation_period_end_date",
                                                overwrite = TRUE)

  results_cohort <- summariseCohortCodeUse(omopgenerics::newCodelist(list(cs = 4134304L)),
                                           cdm = cdm,
                                           cohortTable = "pharyngitis",
                                           timing = "any")

  expect_true(all(omopgenerics::splitAdditional(results_cohort) |>
                    dplyr::filter(variable_name != "overall") |>
                    dplyr::pull("source_concept_name") == "NA"))
  expect_true(all(omopgenerics::splitAdditional(results_cohort) |>
                    dplyr::filter(variable_name != "overall") |>
                    dplyr::pull("source_concept_id") == "NA"))

  CDMConnector::cdmDisconnect(cdm)

})

test_that("empty cohort", {
  skip_on_cran()

  con <- DBI::dbConnect(duckdb::duckdb(),
                        dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con,
                                  cdmSchema = "main",
                                  writeSchema = "main",
                                  cdmName = "test")

  # Empty cohort
  cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                                conceptSet = list(a = 260139L,
                                                                  b = 1127433L),
                                                name = "cohorts",
                                                end = "observation_period_end_date",
                                                overwrite = TRUE)
  results_cohort_mult <- summariseCohortCodeUse(x = omopgenerics::emptyCodelist(),
                                                cdm = cdm,
                                                cohortTable = "cohorts",
                                                timing = "entry")
  expect_true(inherits(results_cohort_mult, "summarised_result"))
  expect_true(nrow(results_cohort_mult) == 0)

  # Source codes
  n1 <- cdm[["condition_occurrence"]] |>
    dplyr::filter(condition_source_concept_id == 35208414) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::pull("n")
  n2 <- cdm[["drug_exposure"]] |>
    dplyr::filter(drug_source_concept_id == 44923712) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::pull("n")

  x <- newCodelist(list("codes" = c(35208414L, 44923712L)))

  expect_no_error(result <- summariseCodeUse(x, cdm, countBy = "record", useSourceCodes = TRUE))
  expect_equal(result |>
                  dplyr::pull("estimate_value") |>
                  as.integer() |>
                  sort(),
                c(n1, n2, n1+n2))

  # summarise cohort code use
  x <- newCodelist(list("codes" = c(4043071L, 40481087L, 19006318L)))
  cdm$new_cohort <- CohortConstructor::conceptCohort(cdm = cdm,
                                                     conceptSet = x,
                                                     name = "new_cohort",
                                                     useSourceFields = TRUE)
  result <- summariseCohortCodeUse(cdm,
                                   cohortTable = "new_cohort",
                                   x = x,
                                   useSourceCodes = TRUE,
                                   countBy = "person")
  n1 <- cdm$procedure_occurrence |>
    dplyr::filter(procedure_source_concept_id == 4043071L) |>
    dplyr::distinct(person_id) |>
    dplyr::tally() |>
    dplyr::pull("n")
  n2 <-  cdm$condition_occurrence |>
    dplyr::filter(condition_source_concept_id == 40481087L) |>
    dplyr::distinct(person_id) |>
    dplyr::tally() |>
    dplyr::pull("n")
  n3 <-  cdm$drug_exposure |>
    dplyr::filter(drug_source_concept_id == 19006318L) |>
    dplyr::distinct(person_id) |>
    dplyr::tally() |>
    dplyr::pull("n")
  expect_equal(result |>
                 dplyr::filter(!is.na(variable_level)) |>
                 dplyr::pull("estimate_value") |>
                 as.integer() |>
                 sort(),
               sort(c(n1, n2, n3)))
  CDMConnector::cdmDisconnect(cdm)

})

