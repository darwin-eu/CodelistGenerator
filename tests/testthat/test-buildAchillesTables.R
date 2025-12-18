test_that("test buildAchillesTables", {
  skip_on_cran()
  con <- duckdb::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(
    con = con, cdmSchema = "main", writeSchema = "main"
  )
  expect_false(any(c("achilles_analysis", "achilles_results", "achilles_results_dist") %in% names(cdm)))
  expect_no_error(cdm <- buildAchillesTables(cdm = cdm))
  expect_true(all(c("achilles_analysis", "achilles_results", "achilles_results_dist") %in% names(cdm)))
  expect_identical(
    cdm$achilles_analysis |> dplyr::pull("analysis_id") |> sort(),
    achillesAnalisisDetails$analysis_id |> sort()
  )
  expect_identical(
    cdm$condition_occurrence |>
      dplyr::filter(condition_concept_id == 4112343) |>
      dplyr::distinct(.data$person_id) |>
      dplyr::tally() |>
      dplyr::pull() |>
      as.integer(),
    cdm$achilles_results |>
      dplyr::filter(.data$stratum_1 == "4112343", .data$analysis_id == 400) |>
      dplyr::pull("count_value")
  )
  expect_identical(
    cdm$condition_occurrence |>
      dplyr::filter(condition_concept_id == 4112343) |>
      dplyr::tally() |>
      dplyr::pull() |>
      as.integer(),
    cdm$achilles_results |>
      dplyr::filter(.data$stratum_1 == "4112343", .data$analysis_id == 401) |>
      dplyr::pull("count_value")
  )
  expect_identical(
    cdm$condition_occurrence |>
      dplyr::filter(condition_source_concept_id == 40479768) |>
      dplyr::tally() |>
      dplyr::pull() |>
      as.integer(),
    cdm$achilles_results |>
      dplyr::filter(.data$stratum_1 == "40479768", .data$analysis_id == 425) |>
      dplyr::pull("count_value")
  )

  omopgenerics::cdmDisconnect(cdm = cdm)
})

test_that("test buildAchillesTables when not all tables required are present", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockConditionOccurrence()

  con = DBI::dbConnect(duckdb::duckdb())

  cdm <- CDMConnector::copyCdmTo(con = con,
                                 cdm = cdm_local,
                                 schema = "main",
                                 overwrite = TRUE)
  attr(cdm, "write_schema") <- "main"

  expect_no_error(buildAchillesTables(cdm))

  omopgenerics::cdmDisconnect(cdm = cdm)
})
