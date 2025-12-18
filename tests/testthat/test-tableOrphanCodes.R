
test_that("test table orphan codes work", {
  skip_on_cran()
  cdm <- mockVocabRef("database")

  codes <- getCandidateCodes(
    cdm = cdm,
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = FALSE
  )
  expect_warning(
    # some cdm tables not present
  orphan_codes <- summariseOrphanCodes(x = omopgenerics::newCodelist(list("msk" = codes$concept_id)),
                                       cdm = cdm)
  )

  expect_no_error(tableOrphanCodes(orphan_codes))
  expect_no_error(tableOrphanCodes(orphan_codes, style = "darwin"))

  # empty result
  expect_warning(
    # some cdm tables not present
  result <- summariseOrphanCodes(omopgenerics::newCodelist(list(a = 99999L)),
                                 cdm = cdm)
  )
  expect_warning(
    # empty result
    tableOrphanCodes(result))

  # not an orphan code use result result
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
  expect_warning(
    # empty result
    tableOrphanCodes(result))

  # With source data
  con <- DBI::dbConnect(duckdb::duckdb(),
                        dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con,
                                  cdmSchema = "main",
                                  writeSchema = "main")
  cdm <- buildAchillesTables(cdm)

  x <- getCandidateCodes(cdm, keywords = "o")
  x <- omopgenerics::newCodelistWithDetails(list("codes" = x))
  expect_warning(result <- summariseOrphanCodes(x, cdm))
  expect_no_error(tab <- tableOrphanCodes(result))
  expect_true("Source concept ID" %in% colnames(tab$`_data`))
  expect_true("Source concept value" %in% colnames(tab$`_data`))
  expect_true("Source concept name" %in% colnames(tab$`_data`))
  CDMConnector::cdmDisconnect(cdm)
})

