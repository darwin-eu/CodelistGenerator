test_that("table achilles code use expcted columns", {
  skip_on_cran()
  # mock db
  cdm <- mockVocabRef("database")

  # two codelists: "Osteoarthritis of knee" "Osteoarthritis of hip"
  result <- summariseAchillesCodeUse(omopgenerics::newCodelist(list(knee_oa = 4L,
                                          hip_oa = 5L)),
                                     cdm = cdm)
  expect_no_error(tableAchillesCodeUse(result,
                         type = "gt",
                         header = c("cdm_name", "estimate_name"),
                         .options = list()))

  expect_no_error(tableAchillesCodeUse(result,
                           type = "gt",
                           header = c("cdm_name", "estimate_name"),
                           style = "darwin",
                           .options = list()))


  expect_no_error(tableAchillesCodeUse(result = result,
                         type = "gt",
                         header = c("codelist_name", "cdm_name", "estimate_name"),
                         hide = c("standard_concept", "standard_concept_id", "vocabulary_id"),
                         .options = list()))

  expect_no_error(tableAchillesCodeUse(result = result,
                                 type = "flextable",
                                 header = c("cdm_name", "estimate_name"),
                                 groupColumn = "codelist_name",
                                 hide = c("standard_concept", "standard_concept_id", "vocabulary_id"),
                                 .options = list()))

  expect_no_error(tableAchillesCodeUse(result = result,
                         type = "flextable",
                         header = c("estimate_name"),
                         groupColumn = "codelist_name",
                         hide = c("cdm_name"),
                         .options = list(includeHeaderName = FALSE)))

  # expected error
  expect_error(tableAchillesCodeUse(result,
                                    type = "gt",
                                    header = c("cdm_name", "estimate_name"),
                                    groupColumn = "estimate_name",
                                    .options = list()))

  # empty result
  result <- summariseAchillesCodeUse(omopgenerics::newCodelist(list(a = 99999L)),
                                     cdm = cdm)
  expect_warning(tableAchillesCodeUse(result))

  # not an achilles result
  cond <- dplyr::tibble("person_id" = 1,
                        "condition_occurrence_id" = 1,
                        "condition_start_date" = as.Date("2000-01-01"),
                        "condition_end_date" = as.Date("2000-01-01"),
                        "condition_type_concept_id" = 1L,
                        "condition_concept_id" = 4,
                        "condition_source_concept_id" = 1,
                        "condition_source_value" = "a")
  cdm <- omopgenerics::insertTable(cdm,
                                   name = "condition_occurrence",
                                   table = cond)
  result <- summariseCodeUse(omopgenerics::newCodelist(list(knee_oa = 4L, hip_oa = 5L)),
                             cdm = cdm)
  expect_warning(tableAchillesCodeUse(result))

  CDMConnector::cdmDisconnect(cdm)

  # With source data
  con <- DBI::dbConnect(duckdb::duckdb(),
                        dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con,
                                  cdmSchema = "main",
                                  writeSchema = "main")
  cdm <- buildAchillesTables(cdm)

  x <- getCandidateCodes(cdm, keywords = "o")
  x <- omopgenerics::newCodelistWithDetails(list("codes" = x))
  result <- summariseAchillesCodeUse(x, cdm)
  expect_no_error(tab <- tableAchillesCodeUse(result))
  expect_true("Source concept ID" %in% colnames(tab$`_data`))
  expect_true("Source concept value" %in% colnames(tab$`_data`))
  expect_true("Source concept name" %in% colnames(tab$`_data`))
  CDMConnector::cdmDisconnect(cdm)
})
