test_that("table code use expcted columns", {
  skip_on_cran()

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main", writeSchema = "main")

  acetiminophen <- c(1125315,  1127433, 40229134,
                     40231925, 40162522, 19133768,  1127078) |>
    as.integer()
  poliovirus_vaccine <- c(40213160L)
  cs <- omopgenerics::newCodelist(list(acetiminophen = acetiminophen,
                                       poliovirus_vaccine = poliovirus_vaccine))

  # Code use
  results <- summariseCodeUse(cs,
                              cdm = cdm,
                              byYear = TRUE,
                              bySex = TRUE,
                              ageGroup = list(c(0,17),
                                              c(18,65),
                                              c(66, 100)))
  expect_no_error(tableCodeUse(result = results))
  expect_no_error(tableCodeUse(result = results,
                               style = "darwin"))
  expect_no_error(tableCodeUse(result = results,
                               type = "gt",
                               header = c("cdm_name", "estimate_name"),
                               groupColumn = NULL,
                               hide = character(),
                               .options = list()))
  expect_no_error(tableCodeUse(result = results,
                               type = "tibble",
                               header = c("cdm_name", "estimate_name"),
                               groupColumn = NULL,
                               hide = "source_concept_id",
                               .options = list()))
  expect_no_error(tableCodeUse(result = results,
                               type = "flextable",
                               header = c("cdm_name", "estimate_name"),
                               groupColumn = "domain_id",
                               .options = list()))

  # Cohort code use
  pharyngitis <- c(4112343L)
  cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                                conceptSet = list(pharyngitis = pharyngitis),
                                                name = "pharyngitis",
                                                end = "observation_period_end_date",
                                                overwrite = TRUE)

  results_cohort <- summariseCohortCodeUse(omopgenerics::newCodelist(list(cs = 4134304L)),
                                           cdm = cdm,
                                           cohortTable = "pharyngitis",
                                           timing = "any")

  tableCohortCodeUse(
    result = results_cohort,
    type = "flextable",
    header = c("cdm_name", "estimate_name"),
    groupColumn = NULL,
    hide = c("source_concept_id", "year"),
    .options = list()
  )

  # group name
  tableCohortCodeUse(
    result = results_cohort,
    type = "flextable",
    header = c("cdm_name", "estimate_name"),
    groupColumn = list("group" = c("cohort_name", "codelist_name")),
    .options = list()
  )

  # timing in combination with concept and source arguments
  tableCohortCodeUse(
    result = results_cohort,
    type = "gt",
    header = c("cdm_name", "estimate_name"),
    groupColumn = list("group" = c("cohort_name", "codelist_name")),
    hide = character(),
    .options = list()
  )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("table code use output formats", {
  skip_on_cran()

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main", writeSchema = "main")

  acetiminophen <- c(1125315,  1127433, 40229134,
                     40231925, 40162522, 19133768,  1127078) |>
    as.integer()
  poliovirus_vaccine <- c(40213160L)
  cs <- omopgenerics::newCodelist(list(acetiminophen = acetiminophen,
                                       poliovirus_vaccine = poliovirus_vaccine))

  results <- summariseCodeUse(cs,
                              cdm = cdm,
                              byYear = TRUE,
                              bySex = TRUE,
                              ageGroup = list(c(0,17),
                                              c(18,65),
                                              c(66, 100)))
  expect_no_error(tableCodeUse(result = results,
                               type = "flextable",
                               header = c("cdm_name", "estimate_name"),
                               groupColumn = "codelist_name",
                               style = "darwin",
                               .options = list()))



  CDMConnector::cdmDisconnect(cdm)
})
