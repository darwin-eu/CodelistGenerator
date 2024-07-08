test_that("table achilles code use expcted columns", {
  # mock db
  cdm <- mockVocabRef("database")

  # two codelists: "Osteoarthritis of knee" "Osteoarthritis of hip"
  result <- summariseAchillesCodeUse(list(knee_oa = 4,
                                          hip_oa = 5),
                                     cdm = cdm)

  tab1 <- tableAchillesCodeUse(result,
                               type = "gt",
                               header = c("cdm_name", "estimate"),
                               conceptId = TRUE,
                               standard = TRUE,
                               vocabulary = TRUE,
                               groupColumns = NULL,
                               excludeColumns = c("result_id", "estimate_type"),
                               .options = list())
  expect_true(inherits(tab1, "gt_tbl"))
  expect_true(all(
    colnames(tab1$`_data`) ==
      c('Codelist name', 'Domain id', 'Standard concept name',
        'Standard concept id', 'Standard concept', 'Vocabulary id',
        '[header]CDM name\n[header_level]mock\n[header_level]Record count')))

  tab2 <- tableAchillesCodeUse(result = result,
                               type = "gt",
                               header = c("group", "cdm_name", "estimate"),
                               conceptId = FALSE,
                               standard = FALSE,
                               vocabulary = FALSE,
                               excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                               .options = list())
  expect_true(inherits(tab2, "gt_tbl"))
  expect_true(all(
    colnames(tab2) ==
      c('Domain id', 'Standard concept name',
        '[header]Codelist name\n[header_level]Knee oa\n[header]CDM name\n[header_level]mock\n[header_level]Record count',
        '[header]Codelist name\n[header_level]Hip oa\n[header]CDM name\n[header_level]mock\n[header_level]Record count')))

  tab3 <- tableAchillesCodeUse(result = result,
                               type = "gt",
                               header = c("group", "cdm_name", "estimate"),
                               conceptId = FALSE,
                               standard = FALSE,
                               vocabulary = FALSE,
                               excludeColumns = c("result_id", "estimate_type"),
                               .options = list())
  expect_true(inherits(tab3, "gt_tbl"))
  expect_true(all(
    colnames(tab3) ==
      c('Domain id', 'Standard concept name',
        '[header]Codelist name\n[header_level]Knee oa\n[header]CDM name\n[header_level]mock\n[header_level]Record count',
        '[header]Codelist name\n[header_level]Hip oa\n[header]CDM name\n[header_level]mock\n[header_level]Record count')))

  tab4 <- tableAchillesCodeUse(result = result,
                               type = "tibble",
                               header = c("estimate"),
                               conceptId = TRUE,
                               standard = TRUE,
                               vocabulary = FALSE,
                               excludeColumns = c("result_id", "estimate_type", "cdm_name"),
                               .options = list())
  expect_false(inherits(tab4, "gt_tbl"))
  expect_false(inherits(tab4, "flextable"))
  expect_true(all(
    colnames(tab4) ==
      c('Codelist name', 'Domain id', 'Standard concept name', 'Standard concept id', 'Standard concept', '[header_level]Record count')))

  tab5 <- tableAchillesCodeUse(result = result,
                               type = "flextable",
                               header = c("estimate"),
                               conceptId = TRUE,
                               standard = TRUE,
                               vocabulary = FALSE,
                               groupColumns = c("cdm_name"),
                               excludeColumns = c("result_id", "estimate_type"),
                               .options = list())
  expect_true(inherits(tab5, "flextable"))
  expect_true(all(
    colnames(tab5$body$dataset) ==
      c('CDM name', 'Codelist name', 'Domain id', 'Standard concept name', 'Standard concept id', 'Standard concept', 'Record count')))
  expect_true(tab5$body$dataset$`CDM name` |> levels() == "mock")

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
                        "condition_source_concept_id" = 1)
  cdm <- omopgenerics::insertTable(cdm,
                                   name = "condition_occurrence",
                                   table = cond)
  result <- summariseCodeUse(list(knee_oa = 4, hip_oa = 5),
                                     cdm = cdm)
  expect_no_error(tableAchillesCodeUse(result))

  CDMConnector::cdm_disconnect(cdm)

})

test_that("test table orphan codes work", {
  cdm <- mockVocabRef("database")

  codes <- getCandidateCodes(
    cdm = cdm,
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = FALSE
  )

  orphan_codes <- summariseOrphanCodes(x = list("msk" = codes$concept_id),
                                       cdm = cdm)

  tab1 <- tableOrphanCodes(orphan_codes)
  expect_true(inherits(tab1, "gt_tbl"))

  # empty result
  result <- summariseOrphanCodes(list(a = 99999),
                                     cdm = cdm)
  expect_no_error(tableOrphanCodes(result))

  # not an orphan code use result result
  cond <- dplyr::tibble("person_id" = 1,
                        "condition_occurrence_id" = 1,
                        "condition_start_date" = as.Date("2000-01-01"),
                        "condition_type_concept_id" = 1L,
                        "condition_concept_id" = 4,
                        "condition_source_concept_id" = 1)
  cdm <- omopgenerics::insertTable(cdm,
                                   name = "condition_occurrence",
                                   table = cond)
  result <- summariseCodeUse(list(knee_oa = 4, hip_oa = 5),
                             cdm = cdm)
  expect_no_error(tableAchillesCodeUse(result))

  CDMConnector::cdm_disconnect(cdm)

})

test_that("expcted behaviour", {
  # mock db
  cdm <- mockVocabRef("database")

  # two codelists: "Osteoarthritis of knee" "Osteoarthritis of hip"
  result <- summariseAchillesCodeUse(list(knee_oa = 4,
                                          hip_oa = 5),
                                     cdm = cdm)

  # exclude additonal but standard and vocaublary TRUE
  expect_error(tableAchillesCodeUse(result,
                                    type = "gt",
                                    header = c("cdm_name", "estimate"),
                                    conceptId = TRUE,
                                    standard = TRUE,
                                    vocabulary = TRUE,
                                    groupColumns = NULL,
                                    excludeColumns = c("additional_name"),
                                    .options = list()))
  expect_error(tableAchillesCodeUse(result,
                                    type = "gt",
                                    header = c("cdm_name", "estimate"),
                                    conceptId = 1,
                                    standard = TRUE,
                                    vocabulary = TRUE,
                                    groupColumns = NULL,
                                    excludeColumns = character(),
                                    .options = list()))
  expect_error(tableAchillesCodeUse(result,
                                    type = "gt",
                                    header = c("cdm_name", "estimate"),
                                    conceptId = TRUE,
                                    standard = 1,
                                    vocabulary = TRUE,
                                    groupColumns = NULL,
                                    excludeColumns = character(),
                                    .options = list()))
  expect_error(tableAchillesCodeUse(result,
                                    type = "gt",
                                    header = c("cdm_name", "estimate"),
                                    conceptId = TRUE,
                                    standard = TRUE,
                                    vocabulary = 0,
                                    groupColumns = NULL,
                                    excludeColumns = character(),
                                    .options = list()))
})
