test_that("table code use expcted columns", {
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

  tab1 <- tableCodeUse(result = results,
                       type = "gt",
                       header = c("cdm_name", "estimate"),
                       splitStrata = FALSE,
                       groupColumn = NULL,
                       conceptId = TRUE,
                       sourceConcept = TRUE,
                       excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                       minCellCount = 5,
                       .options = list())
  expect_true(inherits(tab1, "gt_tbl"))
  expect_true(all(
    colnames(tab1$`_data`) ==
      c('Codelist name', 'Strata name', 'Strata level', 'Standard concept name',
        'Standard concept id', 'Source concept name', 'Source concept id',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Record count',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Person count')))

  tab2 <- tableCodeUse(result = results,
                       type = "tibble",
                       header = c("cdm_name", "estimate"),
                       splitStrata = FALSE,
                       groupColumn = NULL,
                       conceptId = FALSE,
                       sourceConcept = TRUE,
                       excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                       minCellCount = 5,
                       .options = list())
  expect_true(all(
    colnames(tab2) ==
      c('Codelist name', 'Strata name', 'Strata level', 'Standard concept name','Source concept name',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Record count',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Person count')))

  tab3 <- tableCodeUse(result = results,
                       type = "tibble",
                       header = c("cdm_name", "estimate"),
                       splitStrata = FALSE,
                       groupColumn = NULL,
                       conceptId = FALSE,
                       sourceConcept = FALSE,
                       excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                       minCellCount = 5,
                       .options = list())

  expect_true(all(
    colnames(tab3) ==
      c('Codelist name', 'Strata name', 'Strata level', 'Standard concept name',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Record count',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Person count')))

  tab4 <- tableCodeUse(result = results,
                       type = "tibble",
                       header = c("cdm_name", "estimate"),
                       splitStrata = FALSE,
                       groupColumn = NULL,
                       conceptId = TRUE,
                       sourceConcept = FALSE,
                       excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                       minCellCount = 5,
                       .options = list())

  expect_true(all(
    colnames(tab4) ==
      c('Codelist name', 'Strata name', 'Strata level', 'Standard concept name', 'Standard concept id',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Record count',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Person count')))

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

  tab5 <- tableCohortCodeUse(
    result = results_cohort,
    type = "flextable",
    header = c("cdm_name", "estimate"),
    splitStrata = TRUE,
    groupColumn = NULL,
    conceptId = TRUE,
    sourceConcept = FALSE,
    excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
    minCellCount = 5,
    .options = list()
    )
  expect_true(inherits(tab5, "flextable"))
  expect_true(all(
    colnames(tab5$body$dataset) ==
      c('Cohort name', 'Codelist name', 'Standard concept name', 'Standard concept id',
        'CDM name\nSynthea synthetic health database\nRecord count',
        'CDM name\nSynthea synthetic health database\nPerson count')))

  # group name
  tab6 <- tableCohortCodeUse(
    result = results_cohort,
    type = "flextable",
    header = c("cdm_name", "estimate"),
    splitStrata = TRUE,
    groupColumn = list("group" = c("cohort_name", "codelist_name")),
    conceptId = TRUE,
    sourceConcept = FALSE,
    excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
    minCellCount = 5,
    .options = list()
  )
  expect_true(all(
    colnames(tab6$body$dataset) ==
      c('group', 'Standard concept name', 'Standard concept id',
        'CDM name\nSynthea synthetic health database\nRecord count',
        'CDM name\nSynthea synthetic health database\nPerson count')))
  expect_true(tab6$body$dataset$group |> levels() == "Pharyngitis; Cs")

  # timing in combination with concept and source arguments
  tab7 <- tableCohortCodeUse(
    result = results_cohort,
    type = "gt",
    header = c("cdm_name", "estimate"),
    splitStrata = TRUE,
    groupColumn = list("group" = c("cohort_name", "codelist_name")),
    conceptId = TRUE,
    sourceConcept = FALSE,
    timing = TRUE,
    excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
    minCellCount = 5,
    .options = list()
  )
  expect_true(all(
    colnames(tab7$`_data`) ==
      c('group', 'Standard concept name', 'Standard concept id', 'Timing',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Record count',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Person count')))

  tab8 <- tableCohortCodeUse(
    result = results_cohort,
    type = "gt",
    header = c("cdm_name", "estimate"),
    splitStrata = TRUE,
    groupColumn = list("group" = c("cohort_name", "codelist_name")),
    conceptId = TRUE,
    sourceConcept = TRUE,
    timing = TRUE,
    excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
    minCellCount = 5,
    .options = list()
  )
  # issue #128 in visOmopResults will change column order so timing goes after source concept id
  expect_true(all(
    colnames(tab8$`_data`) %in%
      c('group', 'Standard concept name', 'Standard concept id', 'Timing', 'Source concept name', 'Source concept id',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Record count',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Person count')))

  tab9 <- tableCohortCodeUse(
    result = results_cohort,
    type = "gt",
    header = c("cdm_name", "estimate"),
    splitStrata = TRUE,
    groupColumn = list("group" = c("cohort_name", "codelist_name")),
    conceptId = TRUE,
    sourceConcept = TRUE,
    timing = TRUE,
    excludeColumns = c("result_id", "estimate_type"),
    minCellCount = 5,
    .options = list()
  )
  # issue #128 in visOmopResults will change column order so timing goes after domain id
  expect_true(all(
    colnames(tab9$`_data`) %in%
      c('group', 'Standard concept name', 'Standard concept id', 'Timing',
        'Source concept name', 'Source concept id', 'Domain id',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Record count',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Person count')))


  tab10 <- tableCohortCodeUse(
    result = results_cohort,
    type = "gt",
    header = c("cdm_name", "estimate"),
    splitStrata = TRUE,
    groupColumn = list("group" = c("cohort_name", "codelist_name")),
    conceptId = FALSE,
    sourceConcept = FALSE,
    timing = TRUE,
    excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
    minCellCount = 5,
    .options = list()
  )
  expect_true(all(
    colnames(tab10$`_data`) ==
      c('group', 'Standard concept name', 'Timing',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Record count',
        '[header]CDM name\n[header_level]Synthea synthetic health database\n[header_level]Person count')))
})

test_that("table code use output formats", {
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
  tab1 <- tableCodeUse(result = results,
                       type = "flextable",
                       header = c("cdm_name", "estimate"),
                       split = FALSE,
                       groupColumn = "codelist_name",
                       conceptId = TRUE,
                       sourceConcept = FALSE,
                       excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                       minCellCount = 5,
                       .options = list())
  expect_true(inherits(tab1, "flextable"))
  expect_true(all(tab1$body$dataset$`Codelist name`|> levels() == c("Acetiminophen", "Poliovirus vaccine")))
  expect_equal(tab1$body$dataset$`Codelist name`[1:3] |> as.character(),  c("Acetiminophen", NA, NA))

  tab2 <- tableCodeUse(result = results |>
                         dplyr::filter(variable_name == "overall", strata_name == "overall"),
                       type = "gt",
                       header = character(),
                       split = TRUE,
                       groupColumn = "cdm_name",
                       conceptId = TRUE,
                       sourceConcept = FALSE,
                       excludeColumns = c("result_id", "estimate_type",
                                          "variable_name", "variable_level",
                                          "additional_name", "additional_level"),
                       minCellCount = 5,
                       .options = list())
  expect_true(inherits(tab2, "gt_tbl"))
  expect_true(all(tab2$`_data`$`CDM name`|> levels() == c("Synthea synthetic health database")))
  expect_true(all(colnames(tab2$`_data`) ==
                    c("CDM name", "Codelist name", "Estimate name", "Estimate value")))

  tab3 <- tableCodeUse(result = results |>
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
                       minCellCount = 5,
                       .options = list())
  expect_true(inherits(tab3, "gt_tbl"))
  expect_true(all(tab3$`_data`$cdm_name_codelist_name |> levels() ==
                    c("Synthea synthetic health database; Acetiminophen", "Synthea synthetic health database; Poliovirus vaccine")))
  expect_true(all(colnames(tab3$`_data`) ==
                    c("cdm_name_codelist_name", "Estimate name", "Estimate value")))

})

test_that("tableCodeUse expected behaviour", {
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

  # not a summarised result object
  result <- results
  class(result) <- c("tbl_df", "tbl", "data.frame" )
  expect_error(
    tableCodeUse(result = result,
                 type = "tibble",
                 header = c("cdm_name", "estimate"),
                 groupColumn = NULL,
                 conceptId = FALSE,
                 sourceConcept = FALSE,
                 excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                 minCellCount = 5,
                 .options = list())
  )

  # not a grouping column
  expect_error(
    tableCohortCodeUse(result = results,
                 type = "tibble",
                 header = c("cdm_name", "estimate"),
                 groupColumn = c("variable_level"),
                 conceptId = FALSE,
                 sourceConcept = FALSE,
                 excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                 minCellCount = 5,
                 .options = list())
  )

  # not a grouping column
  expect_error(
    tableCodeUse(result = results,
                 type = "tibble",
                 header = c("cdm_name", "estimate"),
                 groupColumn = c("cohort_name"),
                 conceptId = FALSE,
                 sourceConcept = FALSE,
                 excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                 minCellCount = 5,
                 .options = list())
  )

  # not adequate grouping column format
  expect_error(
    tableCodeUse(result = results,
                 type = "tibble",
                 header = c("estimate"),
                 groupColumn = list("cdm_name", "codelist_name"),
                 conceptId = FALSE,
                 sourceConcept = FALSE,
                 excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                 minCellCount = 5,
                 .options = list())
  )

  # .options not a list
  expect_error(
    tableCodeUse(result = results,
                 type = "tibble",
                 header = c("estimate"),
                 groupColumn = c("cdm_name"),
                 conceptId = FALSE,
                 sourceConcept = FALSE,
                 excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                 minCellCount = 5,
                 .options = NULL)
  )

  # not allowed .options
  expect_warning(
    tableCodeUse(result = results,
                 type = "tibble",
                 header = c("estimate"),
                 groupColumn = c("cdm_name"),
                 conceptId = FALSE,
                 sourceConcept = FALSE,
                 excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                 minCellCount = 5,
                 .options = list("hola" = 1))
  )

})
