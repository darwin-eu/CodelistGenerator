test_that("getATCCodes working", {

  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backend = backends[i])

    atcCodes <- getATCCodes(cdm, level = "ATC 1st")
    expect_true("codelist" %in% class(atcCodes))
    expect_true(all(atcCodes[[1]] == c(12,13)))
    expect_true(c("1234_alimentary_tract_and_metabolism") %in%
                  names(atcCodes))
    expect_true(inherits(atcCodes, "codelist"))

    atcCodes2 <- getATCCodes(cdm, level = "ATC 1st",
                             name = "ALIMENTARY TRACT AND METABOLISM")
    expect_true(all(atcCodes2[[1]] == c(12,13)))

    atcCodes3 <- getATCCodes(cdm, level = "ATC 1st",
                             name = "ALIMENTARY TRACT AND METABOLISM",
                             type = "codelist_with_details")
    expect_true(!is.null(atcCodes3[[1]]$concept_name))
    expect_true(inherits(atcCodes3, "codelist_with_details"))
    expect_identical(names(atcCodes3), "1234_alimentary_tract_and_metabolism")

    atcCodes3 <- getATCCodes(cdm, level = "ATC 1st",
                             name = "ALIMENTARY TRACT AND METABOLISM",
                             nameStyle = "{concept_id}_{concept_name}")
    expect_identical(names(atcCodes3), "12_alimentary_tract_and_metabolism")
    expect_true(all(atcCodes3[[1]] == c(12,13)))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})

test_that("getATCCodes expected errors", {

  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backend = backends[i])
    expect_error(getATCCodes(cdm, level = "Not an ATC level"))
    expect_error(getATCCodes(cdm, level = "ATC 1st",
                             name = "Not an ATC name"))
    expect_error(getATCCodes(cdm, nameStyle = "hello"))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }

})

test_that("getDrugIngredientCodes working", {

  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backend = backends[i])

    ing_codes0 <- getDrugIngredientCodes(cdm, nameStyle = "{concept_code}_{concept_name}")
    expect_true(all(ing_codes0[[1]] == c(10,13)))
    expect_true(inherits(ing_codes0, "codelist"))
    expect_identical(names(ing_codes0),  c("1234_adalimumab", "1234_other_ingredient"))

    ing_codes1 <- getDrugIngredientCodes(cdm, nameStyle = "{concept_id}_{concept_name}")
    expect_true(all(ing_codes1[[1]] == c(10,13)))
    expect_identical(names(ing_codes1),  c("10_adalimumab", "19_other_ingredient"))

    ing_codes2 <- getDrugIngredientCodes(cdm, name = "Adalimumab")
    expect_true(all(ing_codes2[[1]] == c(10,13)))
    expect_true(names(ing_codes2) == "1234_adalimumab")

    ing_codes2 <- getDrugIngredientCodes(cdm, name = 10)
    expect_true(all(ing_codes2[[1]] == c(10,13)))
    expect_true(names(ing_codes2) == "1234_adalimumab")

    ing_codes3 <- getDrugIngredientCodes(cdm,
                                         name = "Adalimumab",
                                         doseForm = "injectable")
    expect_true(all(ing_codes3[[1]] == c(13)))

    ing_codes4 <- getDrugIngredientCodes(cdm,
                                         name = "Adalimumab",
                                         doseForm = "injection")
    expect_true(all(ing_codes4[[1]] == c(10)))

    ing_codes5 <- getDrugIngredientCodes(cdm,
                                         name = "Adalimumab",
                                         doseForm = "injection",
                                         type = "codelist_with_details")
    expect_true(!is.null(ing_codes5[[1]]$concept_name))
    expect_true(inherits(ing_codes5, "codelist_with_details"))

    # limiting on ingredients
    ing_codes_all <- getDrugIngredientCodes(cdm,
                                            ingredientRange = c(1,Inf))
    ing_codes_mono <- getDrugIngredientCodes(cdm,
                                             ingredientRange = c(1,1))
    ing_codes_comb <- getDrugIngredientCodes(cdm,
                                             ingredientRange = c(2,Inf))

    expect_equal(ing_codes_all, ing_codes0)
    expect_true(all(c(10) %in% ing_codes_mono$`1234_adalimumab`))
    expect_null(ing_codes_mono$`1234_other_ingredient`)
    expect_true(all(c(13) %in% ing_codes_comb$`1234_adalimumab`))
    expect_true(all(c(13) %in% ing_codes_comb$`1234_other_ingredient`))

    expect_error(getDrugIngredientCodes(cdm,
                                        ingredientRange = c(3,2)))
    expect_error(getDrugIngredientCodes(cdm,
                                        ingredientRange = c(3)))
    expect_error(getDrugIngredientCodes(cdm,
                                        ingredientRange = c("a", "b")))
    expect_error(getDrugIngredientCodes(cdm,
                                        ingredientRange = c(-1, 25)))


    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }

  }
})

test_that("getDrugIngredientCodes expected errors", {

  backends <- c("database","data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backend = backends[i])
    expect_error(getDrugIngredientCodes(cdm, name = "Not an Ingredient"))
    expect_error(getDrugIngredientCodes(cdm, name = -99))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }

})

test_that("no duplicate names example 1",{
  skip_on_cran()
  person <- dplyr::tibble(
    person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
    race_concept_id = 0, ethnicity_concept_id = 0
  )
  observationPeriod <- dplyr::tibble(
    observation_period_id = 1, person_id = 1,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2025-12-31"),
    period_type_concept_id = 0
  )
  concept <- data.frame(
    concept_id = 1:19,
    concept_name = c(
      "Musculoskeletal disorder",
      "Osteoarthrosis",
      "Arthritis",
      "Osteoarthritis of knee",
      "Osteoarthritis of hip",
      "Osteonecrosis",
      "Degenerative arthropathy",
      "Knee osteoarthritis",
      "H/O osteoarthritis",
      "Adalimumab",
      "Injection",
      "ALIMENTARY TRACT AND METABOLISM",
      "Descendant drug",
      "Injectable",
      "Diseases of the musculoskeletal system and connective tissue",
      "Arthropathies",
      "Arthritis",
      "OA",
      "Adalimumab"
    ),
    domain_id = c(rep("Condition", 8), "Observation", rep("Drug", 5),
                  rep("Condition", 4), "Drug"),
    vocabulary_id = c(
      rep("SNOMED", 6),
      rep("Read", 2),
      "LOINC", "RxNorm", "OMOP",
      "ATC",
      "RxNorm", "OMOP",
      "ICD10", "ICD10", "ICD10", "ICD10", "RxNorm"
    ),
    standard_concept = c(
      rep("S", 6),
      rep(NA, 2),
      "S", "S", NA,
      NA, "S", NA, NA, NA, NA, NA, "S"
    ),
    concept_class_id = c(
      rep("Clinical Finding", 6),
      rep("Diagnosis", 2),
      "Observation", "Ingredient", "Dose Form",
      "ATC 1st", "Drug", "Dose Form",
      "ICD10 Chapter", "ICD10 SubChapter",
      "ICD Code","ICD Code", "Ingredient"
    ),
    concept_code = as.character(c(2:20)),
    valid_start_date = NA,
    valid_end_date = NA,
    invalid_reason =
      NA
  )
  conceptAncestor <- dplyr::bind_rows(
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 1L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 3L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 2L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 3L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 4L,
      min_levels_of_separation = 2,
      max_levels_of_separation = 2
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 5L,
      min_levels_of_separation = 2,
      max_levels_of_separation = 2
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 4L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 5L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 10L,
      descendant_concept_id = 10L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 10L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 12L,
      descendant_concept_id = 12L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 12L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 19L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    )
  )
  conceptSynonym <- dplyr::bind_rows(
    data.frame(
      concept_id = 2L,
      concept_synonym_name = "Arthritis"
    ),
    data.frame(
      concept_id = 3L,
      concept_synonym_name = "Osteoarthrosis"
    )
  )|>
    dplyr::mutate(language_concept_id  = NA)

  vocabulary <- dplyr::bind_rows(
    data.frame(
      vocabulary_id = "SNOMED",
      vocabulary_name = "SNOMED",
      vocabulary_reference = "1",
      vocabulary_version = "1",
      vocabulary_concept_id = 1
    ),
    data.frame(
      vocabulary_id = "None",
      vocabulary_name = "OMOP Standardized Vocabularies",
      vocabulary_reference = "Omop generated",
      vocabulary_version = "v5.0 22-JUN-22",
      vocabulary_concept_id = 44819096
    )
  )

  drugStrength <- dplyr::bind_rows(
    data.frame(
      drug_concept_id = 10L,
      ingredient_concept_id = 10L,
      amount_value = NA,
      amount_unit_concept_id = 8576,
      numerator_value = 0.010,
      numerator_unit_concept_id = 8576,
      denominator_value = 0.5,
      denominator_unit_concept_id = 8587,
      box_size = NA,
      valid_start_date = NA,
      valid_end_date = NA
    )
  )

  cdmSource <- dplyr::as_tibble(
    data.frame(
      cdm_source_name  = "mock",
      cdm_source_abbreviation = NA,
      cdm_holder = NA,
      source_description = NA,
      source_documentation_reference = NA,
      cdm_etl_reference = NA,
      source_release_date = NA,
      cdm_release_date = NA,
      cdm_version = "5.3",
      vocabulary_version  = NA
    )
  )

  conceptRelationship <- dplyr::bind_rows(
    data.frame(
      concept_id_1 = 2L,
      concept_id_2 = 7L,
      relationship_id = "Mapped from"
    ),
    data.frame(
      concept_id_1 = 4L,
      concept_id_2 = 8L,
      relationship_id = "Mapped from"
    ),
    data.frame(
      concept_id_1 = 10L,
      concept_id_2 = 11L,
      relationship_id = "RxNorm has dose form"
    ),
    data.frame(
      concept_id_1 = 3L,
      concept_id_2 = 6L,
      relationship_id = "Due to of"
    ),
    data.frame(
      concept_id_1 = 13L,
      concept_id_2 = 14L,
      relationship_id = "RxNorm has dose form"
    ),
    data.frame(
      concept_id_1 = 15L,
      concept_id_2 = 16L,
      relationship_id = "Subsumes"
    ),
    data.frame(
      concept_id_1 = 16L,
      concept_id_2 = 17L,
      relationship_id = "Subsumes"
    ),
    data.frame(
      concept_id_1 = 17L,
      concept_id_2 = 18L,
      relationship_id = "Subsumes"
    ),
    data.frame(
      concept_id_1 = 18L,
      concept_id_2 = 3L,
      relationship_id = "Maps to"
    )
  ) |>
    dplyr::mutate(valid_start_date = NA,
                  valid_end_date = NA,
                  invalid_reason = NA)

  cdm_df <- omopgenerics::cdmFromTables(tables = list(person = person,
                                                      concept = concept,
                                                      concept_ancestor = conceptAncestor,
                                                      concept_synonym = conceptSynonym,
                                                      concept_relationship = conceptRelationship,
                                                      vocabulary = vocabulary,
                                                      drug_strength = drugStrength,
                                                      observation_period = observationPeriod,
                                                      cdm_source = cdmSource),
                                        cdmName = "mock")

  # into db
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  cdm <- CDMConnector::copyCdmTo(con = db,
                                 cdm = cdm_df,
                                 schema = "main",
                                 overwrite = TRUE)
  attr(cdm, "write_schema") <- "main"

  expect_no_error(getDrugIngredientCodes(
    cdm = cdm, name = "Adalimumab", nameStyle = "{concept_name}"
  ))

  expect_no_error(getDrugIngredientCodes(
    cdm = cdm, name = "Adalimumab", nameStyle = "{concept_code}"
  ))

  ingredient_list <- getDrugIngredientCodes(
    cdm = cdm, name = "Adalimumab"
  )

  expect_equal(names(ingredient_list) |>
                 length() |>
                 as.numeric(),
               2
  )

  expect_equal(names(ingredient_list) |>
                 unique()|>
                 length() |>
                 as.numeric(),
               2
  )

  expect_false(all(startsWith(names(ingredient_list), "a")))

  expect_true(all(names(ingredient_list)
                  %in%
                    c("11_adalimumab", "20_adalimumab")))

  cdm <- mockVocabRef()

  ingredient_list <- getDrugIngredientCodes(
    cdm = cdm, name = "Adalimumab"
  )

  expect_false(all(startsWith(names(ingredient_list), "a")))

  expect_true(all(names(ingredient_list)
                  %in%
                    c("1234_adalimumab")))
})

test_that("no duplicate names example 2",{
  skip_on_cran()
  person <- dplyr::tibble(
    person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
    race_concept_id = 0, ethnicity_concept_id = 0
  )
  observationPeriod <- dplyr::tibble(
    observation_period_id = 1, person_id = 1,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2025-12-31"),
    period_type_concept_id = 0
  )
  concept <- data.frame(
    concept_id = 1:19,
    concept_name = c(
      "Musculoskeletal disorder",
      "Osteoarthrosis",
      "Arthritis",
      "Osteoarthritis of knee",
      "Osteoarthritis of hip",
      "Osteonecrosis",
      "Degenerative arthropathy",
      "Knee osteoarthritis",
      "H/O osteoarthritis",
      "Adalimumab",
      "Injection",
      "ALIMENTARY TRACT AND METABOLISM",
      "Descendant drug",
      "Injectable",
      "Diseases of the musculoskeletal system and connective tissue",
      "Arthropathies",
      "Arthritis",
      "OA",
      "ALIMENTARY TRACT AND METABOLISM"
    ),
    domain_id = c(rep("Condition", 8), "Observation", rep("Drug", 5),
                  rep("Condition", 4), "Drug"),
    vocabulary_id = c(
      rep("SNOMED", 6),
      rep("Read", 2),
      "LOINC", "RxNorm", "OMOP",
      "ATC",
      "RxNorm", "OMOP",
      "ICD10", "ICD10", "ICD10", "ICD10", "ATC"
    ),
    standard_concept = c(
      rep("S", 6),
      rep(NA, 2),
      "S", "S", NA,
      NA, "S", NA, NA, NA, NA, NA, "S"
    ),
    concept_class_id = c(
      rep("Clinical Finding", 6),
      rep("Diagnosis", 2),
      "Observation", "Ingredient", "Dose Form",
      "ATC 1st", "Drug", "Dose Form",
      "ICD10 Chapter", "ICD10 SubChapter",
      "ICD Code","ICD Code", "ATC 1st"
    ),
    concept_code = as.character(c(2:20)),
    valid_start_date = NA,
    valid_end_date = NA,
    invalid_reason =
      NA
  )
  conceptAncestor <- dplyr::bind_rows(
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 1L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 3L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 2L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 3L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 4L,
      min_levels_of_separation = 2,
      max_levels_of_separation = 2
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 5L,
      min_levels_of_separation = 2,
      max_levels_of_separation = 2
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 4L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 5L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 10L,
      descendant_concept_id = 10L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 10L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 12L,
      descendant_concept_id = 12L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 12L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 19L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    )
  )
  conceptSynonym <- dplyr::bind_rows(
    data.frame(
      concept_id = 2L,
      concept_synonym_name = "Arthritis"
    ),
    data.frame(
      concept_id = 3L,
      concept_synonym_name = "Osteoarthrosis"
    )
  )|>
    dplyr::mutate(language_concept_id  = NA)

  conceptRelationship <- dplyr::bind_rows(
    data.frame(
      concept_id_1 = 2L,
      concept_id_2 = 7L,
      relationship_id = "Mapped from"
    ),
    data.frame(
      concept_id_1 = 4L,
      concept_id_2 = 8L,
      relationship_id = "Mapped from"
    ),
    data.frame(
      concept_id_1 = 10L,
      concept_id_2 = 11L,
      relationship_id = "RxNorm has dose form"
    ),
    data.frame(
      concept_id_1 = 3L,
      concept_id_2 = 6L,
      relationship_id = "Due to of"
    ),
    data.frame(
      concept_id_1 = 13L,
      concept_id_2 = 14L,
      relationship_id = "RxNorm has dose form"
    ),
    data.frame(
      concept_id_1 = 15L,
      concept_id_2 = 16L,
      relationship_id = "Subsumes"
    ),
    data.frame(
      concept_id_1 = 16L,
      concept_id_2 = 17L,
      relationship_id = "Subsumes"
    ),
    data.frame(
      concept_id_1 = 17L,
      concept_id_2 = 18L,
      relationship_id = "Subsumes"
    ),
    data.frame(
      concept_id_1 = 18L,
      concept_id_2 = 3L,
      relationship_id = "Maps to"
    )
  ) |>
    dplyr::mutate(valid_start_date = NA,
                  valid_end_date = NA,
                  invalid_reason = NA)

  vocabulary <- dplyr::bind_rows(
    data.frame(
      vocabulary_id = "SNOMED",
      vocabulary_name = "SNOMED",
      vocabulary_reference = "1",
      vocabulary_version = "1",
      vocabulary_concept_id = 1
    ),
    data.frame(
      vocabulary_id = "None",
      vocabulary_name = "OMOP Standardized Vocabularies",
      vocabulary_reference = "Omop generated",
      vocabulary_version = "v5.0 22-JUN-22",
      vocabulary_concept_id = 44819096
    )
  )

  drugStrength <- dplyr::bind_rows(
    data.frame(
      drug_concept_id = 10L,
      ingredient_concept_id = 10L,
      amount_value = NA,
      amount_unit_concept_id = 8576,
      numerator_value = 0.010,
      numerator_unit_concept_id = 8576,
      denominator_value = 0.5,
      denominator_unit_concept_id = 8587,
      box_size = NA,
      valid_start_date = NA,
      valid_end_date = NA
    )
  )

  cdmSource <- dplyr::as_tibble(
    data.frame(
      cdm_source_name  = "mock",
      cdm_source_abbreviation = NA,
      cdm_holder = NA,
      source_description = NA,
      source_documentation_reference = NA,
      cdm_etl_reference = NA,
      source_release_date = NA,
      cdm_release_date = NA,
      cdm_version = "5.3",
      vocabulary_version  = NA
    )
  )

  cdm_df <- omopgenerics::cdmFromTables(tables = list(person = person,
                                                      concept = concept,
                                                      concept_ancestor = conceptAncestor,
                                                      concept_synonym = conceptSynonym,
                                                      concept_relationship = conceptRelationship,
                                                      vocabulary = vocabulary,
                                                      drug_strength = drugStrength,
                                                      observation_period = observationPeriod,
                                                      cdm_source = cdmSource),
                                        cdmName = "mock")

  # into db
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  cdm <- CDMConnector::copyCdmTo(con = db,
                                 cdm = cdm_df,
                                 schema = "main",
                                 overwrite = TRUE)
  attr(cdm, "write_schema") <- "main"

  atc_list <- getATCCodes(
    cdm = cdm, name = "ALIMENTARY TRACT AND METABOLISM"
  )

  expect_equal(names(atc_list) |>
                 length() |>
                 as.numeric(),
               2
  )

  expect_equal(names(atc_list) |>
                 unique()|>
                 length() |>
                 as.numeric(),
               2
  )

  expect_false(all(startsWith(names(atc_list), "a")))
  expect_true(startsWith(names(atc_list)[1], "1"))
  expect_true(startsWith(names(atc_list)[2], "2"))

  expect_true(all(names(atc_list)
                  %in%
                    c("13_alimentary_tract_and_metabolism", "20_alimentary_tract_and_metabolism")))

  cdm <- mockVocabRef()

  atc_list <- getATCCodes(
    cdm = cdm, name = "ALIMENTARY TRACT AND METABOLISM"
  )

  expect_false(all(startsWith(names(atc_list), "a")))
  expect_true(all(startsWith(names(atc_list), "1")))
})
