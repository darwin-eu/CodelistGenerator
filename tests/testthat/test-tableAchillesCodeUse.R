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
                                       cdm = cdm,
                                       domains = "Condition",
                                       standardConcept = "Standard",
                                       searchInSynonyms = FALSE,
                                       searchNonStandard = FALSE,
                                       includeDescendants = TRUE,
                                       includeAncestor = FALSE)

  # different combinations of settings and additoinal variables:
  tab1 <- tableOrphanCodes(orphan_codes,
                           type = "gt",
                           header = c("cdm_name", "estimate"),
                           conceptId = TRUE,
                           standard = TRUE,
                           vocabulary = TRUE,
                           relationship = FALSE,
                           settings = c("search_in_synonyms"),
                           groupColumns = NULL,
                           excludeColumns = c("result_id", "estimate_type"),
                           .options = list())
  expect_true(inherits(tab1, "gt_tbl"))
  expect_true(all(
    colnames(tab1$`_data`) ==
      c('Codelist name', 'Domain id', 'Standard concept name', 'Standard concept id',
        'Standard concept', 'Vocabulary id', 'Search in synonyms',
        '[header]CDM name\n[header_level]mock\n[header_level]Record count')))

  tab2 <- tableOrphanCodes(orphan_codes,
                           type = "gt",
                           header = c("cdm_name", "estimate"),
                           conceptId = TRUE,
                           standard = FALSE,
                           vocabulary = TRUE,
                           relationship = FALSE,
                           settings = c("search_in_synonyms"),
                           groupColumns = NULL,
                           excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                           .options = list())
  expect_true(inherits(tab2, "gt_tbl"))
  expect_true(all(
    colnames(tab2$`_data`) ==
      c('Codelist name', 'Domain id', 'Standard concept name', 'Standard concept id',
        'Vocabulary id', 'Search in synonyms',
        '[header]CDM name\n[header_level]mock\n[header_level]Record count')))

  tab3 <- tableOrphanCodes(orphan_codes,
                           type = "flextable",
                           header = c("cdm_name", "estimate"),
                           conceptId = TRUE,
                           standard = TRUE,
                           vocabulary = FALSE,
                           relationship = FALSE,
                           settings = c("search_in_synonyms", "search_standard_concept"),
                           groupColumns = NULL,
                           excludeColumns = c("result_id", "estimate_type"),
                           .options = list())
  expect_true(inherits(tab3, "flextable"))
  expect_true(all(
    colnames(tab3$body$dataset) ==
      c('Codelist name', 'Domain id', 'Standard concept name', 'Standard concept id',
        'Standard concept', 'Search in synonyms', 'Search standard concept',
        'CDM name\nmock\nRecord count')))

  tab4 <- tableOrphanCodes(orphan_codes,
                           type = "tibble",
                           header = c("cdm_name", "estimate"),
                           conceptId = TRUE,
                           standard = TRUE,
                           vocabulary = FALSE,
                           relationship = TRUE,
                           settings = c("search_in_synonyms", "search_standard_concept"),
                           groupColumns = NULL,
                           excludeColumns = c("result_id", "estimate_type"),
                           .options = list())
  expect_true(all(
    colnames(tab4) ==
      c('Codelist name', 'Domain id', 'Standard concept name', 'Standard concept id',
        'Standard concept', 'Relationship id', 'Search in synonyms', 'Search standard concept',
        '[header]CDM name\n[header_level]mock\n[header_level]Record count')))

  tab5 <- tableOrphanCodes(orphan_codes,
                           type = "tibble",
                           header = c("cdm_name", "estimate"),
                           conceptId = FALSE,
                           standard = FALSE,
                           vocabulary = FALSE,
                           relationship = TRUE,
                           settings = character(),
                           groupColumns = NULL,
                           excludeColumns = c("result_id", "estimate_type"),
                           .options = list())
  expect_true(all(
    colnames(tab5) ==
      c('Codelist name', 'Domain id', 'Standard concept name', 'Relationship id',
        '[header]CDM name\n[header_level]mock\n[header_level]Record count')))

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
