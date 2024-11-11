test_that("unmapped codes", {

  cdm <- mockVocabRef("database")
  cdm <- omopgenerics::insertTable(cdm, "condition_occurrence",
              dplyr::tibble(person_id = NA,
                            condition_occurrence_id = NA,
                            condition_concept_id = NA,
                            condition_start_date = NA,
                            condition_type_concept_id = NA,
                            condition_source_concept_id = NA))
  cdm <- omopgenerics::insertTable(cdm, "drug_exposure",
                     dplyr::tibble(person_id = NA,
                                   drug_exposure_id = NA,
                                   drug_concept_id = NA,
                                   drug_exposure_start_date = NA,
                                   drug_exposure_end_date = NA,
                                   drug_type_concept_id = NA,
                                   drug_source_concept_id = NA))
  cdm <- omopgenerics::insertTable(cdm, "observation",
                     dplyr::tibble(person_id = NA,
                                   observation_id = NA,
                                   observation_concept_id = NA,
                                   observation_date = NA,
                                   observation_type_concept_id = NA,
                                   observation_source_concept_id = NA))
  cdm <- omopgenerics::insertTable(cdm, "procedure_occurrence",
                     dplyr::tibble(person_id = NA,
                                   procedure_occurrence_id = NA,
                                   procedure_concept_id = NA,
                                   procedure_date  = NA,
                                   procedure_type_concept_id  = NA,
                                   procedure_source_concept_id = NA))
  cdm <- omopgenerics::insertTable(cdm, "device_exposure",
                     dplyr::tibble(person_id = NA,
                                   device_exposure_id = NA,
                                   device_concept_id = NA,
                                   device_exposure_start_date  = NA,
                                   device_type_concept_id  = NA,
                                   device_source_concept_id = NA))
  cdm <- omopgenerics::insertTable(cdm, "measurement",
                     dplyr::tibble(person_id = NA,
                                   measurement_id = NA,
                                   measurement_concept_id = NA,
                                   measurement_date  = NA,
                                   measurement_type_concept_id  = NA,
                                   measurement_source_concept_id = NA))

  # no unmapped codes to find
  expect_no_error(unmappedCodes <- summariseUnmappedCodes(
    x = list("cs" = 1L),
    cdm = cdm))
  expect_true(nrow(unmappedCodes) == 0)

  cdm <- omopgenerics::insertTable(cdm, "measurement",
                     dplyr::tibble(person_id = 1,
                                   measurement_id = 1,
                                   measurement_concept_id = 0,
                                   measurement_date  = as.Date("2000-01-01"),
                                   measurement_type_concept_id  = NA,
                                   measurement_source_concept_id = 7))

  startTables <- CDMConnector::listTables(attr(attr(cdm, "cdm_source"), "dbcon"),
                                          schema = attr(cdm, "write_schema")
  )
  expect_no_error(unmappedCodes <- summariseUnmappedCodes(
    x = list("cs" = 2),
    cdm = cdm))
  endTables <- CDMConnector::listTables(attr(attr(cdm, "cdm_source"), "dbcon"),
                                          schema = attr(cdm, "write_schema")
  )
  expect_identical(startTables, endTables)
  # we now have an unmapped code
  expect_true(nrow(unmappedCodes) == 1)
  expect_true(unmappedCodes$estimate_value == "1")

  # we would miss it if we don't look in the measurement table
  expect_no_error(unmappedCodes <- summariseUnmappedCodes(
    x = list("cs" = 2),
    cdm = cdm, table = "condition_occurrence"))
  expect_true(nrow(unmappedCodes) == 0)


})
