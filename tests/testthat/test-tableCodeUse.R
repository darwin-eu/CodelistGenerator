test_that("table code use expcted columns", {
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

  # Code use
  results <- summariseCodeUse(cs,
                              cdm = cdm,
                              byYear = TRUE,
                              bySex = TRUE,
                              ageGroup = list(c(0,17),
                                              c(18,65),
                                              c(66, 100)))

  tableCodeUse(result = results,
               type = "gt",
               header = c("cdm_name", "estimate_name"),
               groupColumn = NULL,
               hide = character(),
               .options = list())

  tableCodeUse(result = results,
               type = "tibble",
               header = c("cdm_name", "estimate_name"),
               groupColumn = NULL,
               hide = "source_concept_id",
               .options = list())

  tableCodeUse(result = results,
               type = "flextable",
               header = c("cdm_name", "estimate_name"),
               groupColumn = "domain_id",
               .options = list())

  # Cohort code use
  pharyngitis <- c(4112343)
  cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                                conceptSet = list(pharyngitis = pharyngitis),
                                                name = "pharyngitis",
                                                end = "observation_period_end_date",
                                                overwrite = TRUE)

  results_cohort <- summariseCohortCodeUse(list(cs = 4134304),
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
    timing = TRUE,
    .options = list()
  )

  tableCohortCodeUse(
    result = results_cohort,
    type = "gt",
    header = c("cdm_name", "estimate_name"),
    groupColumn = "timing",
    timing = TRUE,
    excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
    .options = list()
  )

  # empty result
  results <- summariseCodeUse(list(a = 99999), cdm = cdm)
  tableCodeUse(result = results)

  # no code use result
  results <- summariseOrphanCodes(x = list("msk" = 1), cdm = cdm)
  tableCodeUse(result = results)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("table code use output formats", {
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
  tableCodeUse(result = results,
               type = "flextable",
               header = c("cdm_name", "estimate_name"),
               groupColumn = "codelist_name",
               .options = list())

  tableCodeUse(result = results |>  dplyr::filter(variable_name == "overall", strata_name == "overall"),
               type = "gt",
               header = character(),
               split = TRUE,
               groupColumn = "cdm_name",
               conceptId = TRUE,
               sourceConcept = FALSE,
               excludeColumns = c("result_id", "estimate_type",
                                  "variable_name", "variable_level",
                                  "additional_name", "additional_level"),
               .options = list())

  tableCodeUse(result = results |>
                 dplyr::filter(variable_name == "overall", strata_name == "overall"),
               type = "gt",
               header = character(),
               split = TRUE,
               groupColumn = c("cdm_name", "codelist_name"),
               conceptId = TRUE,
               sourceConcept = FALSE,
               excludeColumns = c("result_id", "estimate_type",
                                  "variable_name", "variable_level",
                                  "additional_name", "additional_level"),
               .options = list())

  CDMConnector::cdm_disconnect(cdm)
})
