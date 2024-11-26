test_that("table achilles code use expcted columns", {
  skip_on_cran()
  # mock db
  cdm <- mockVocabRef("database")

  # two codelists: "Osteoarthritis of knee" "Osteoarthritis of hip"
  result <- summariseAchillesCodeUse(list(knee_oa = 4,
                                          hip_oa = 5),
                                     cdm = cdm)
      tableAchillesCodeUse(result,
                         type = "gt",
                         header = c("cdm_name", "estimate_name"),
                         .options = list())

    tableAchillesCodeUse(result = result,
                         type = "gt",
                         header = c("codelist_name", "cdm_name", "estimate_name"),
                         hide = c("standard_concept", "standard_concept_id", "vocabulary_id"),
                         .options = list())

    tableAchillesCodeUse(result = result,
                                 type = "flextable",
                                 header = c("cdm_name", "estimate_name"),
                                 groupColumn = "codelist_name",
                                 hide = c("standard_concept", "standard_concept_id", "vocabulary_id"),
                                 .options = list())

    tableAchillesCodeUse(result = result,
                         type = "flextable",
                         header = c("estimate_name"),
                         groupColumn = "codelist_name",
                         hide = c("cdm_name"),
                         .options = list(includeHeaderName = FALSE))

  # expected error
  expect_error(tableAchillesCodeUse(result,
                                    type = "gt",
                                    header = c("cdm_name", "estimate_name"),
                                    groupColumn = "estimate_name",
                                    .options = list()))

  # empty result
  result <- summariseAchillesCodeUse(list(a = 99999),
                                     cdm = cdm)
  expect_no_error(tableAchillesCodeUse(result))

  # not an achilles result
  cond <- dplyr::tibble("person_id" = 1,
                        "condition_occurrence_id" = 1,
                        "condition_start_date" = as.Date("2000-01-01"),
                        "condition_type_concept_id" = 1L,
                        "condition_concept_id" = 4,
                        "condition_source_concept_id" = 1,
                        "condition_source_value" = "a")
  cdm <- omopgenerics::insertTable(cdm,
                                   name = "condition_occurrence",
                                   table = cond)
  result <- summariseCodeUse(list(knee_oa = 4, hip_oa = 5),
                             cdm = cdm)
  expect_no_error(tableAchillesCodeUse(result))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("test table orphan codes work", {
  skip_on_cran()
  cdm <- mockVocabRef("database")

  codes <- getCandidateCodes(
    cdm = cdm,
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = FALSE
  )

  orphan_codes <- summariseOrphanCodes(x = list("msk" = codes$concept_id),
                                       cdm = cdm)

  tableOrphanCodes(orphan_codes)

  # empty result
  result <- summariseOrphanCodes(list(a = 99999),
                                 cdm = cdm)
  tableOrphanCodes(result)

  # not an orphan code use result result
  cond <- dplyr::tibble("person_id" = 1,
                        "condition_occurrence_id" = 1,
                        "condition_start_date" = as.Date("2000-01-01"),
                        "condition_type_concept_id" = 1L,
                        "condition_concept_id" = 4,
                        "condition_source_concept_id" = 1,
                        "condition_source_value" = "a")
  cdm <- omopgenerics::insertTable(cdm,
                                   name = "condition_occurrence",
                                   table = cond)
  result <- summariseCodeUse(list(knee_oa = 4, hip_oa = 5),
                             cdm = cdm)
  tableAchillesCodeUse(result)

  CDMConnector::cdm_disconnect(cdm)
})

