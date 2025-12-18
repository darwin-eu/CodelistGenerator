test_that("getATCCodes working", {
  skip_on_cran()
  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backend = backends[i])

    atcCodes <- getATCCodes(cdm, level = "ATC 1st")
    expect_true("codelist" %in% class(atcCodes))
    expect_true(all(atcCodes[[1]] == c(12L, 13L)))
    expect_true(c("1234_alimentary_tract_and_metabolism") %in%
                  names(atcCodes))
    expect_true(inherits(atcCodes, "codelist"))

    atcCodes2 <- getATCCodes(cdm, level = "ATC 1st",
                             name = "ALIMENTARY TRACT AND METABOLISM")
    expect_true(all(atcCodes2[[1]] == c(12L, 13L)))

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
    expect_true(all(atcCodes3[[1]] == c(12L, 13L)))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})

test_that("getATCCodes expected errors", {
  skip_on_cran()
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
  skip_on_cran()
  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backend = backends[i])

    ing_codes0 <- getDrugIngredientCodes(cdm, nameStyle = "{concept_code}_{concept_name}")
    expect_true(all(ing_codes0[[1]] == c(10L, 13L)))
    expect_true(inherits(ing_codes0, "codelist"))
    expect_identical(names(ing_codes0),  c("1234_adalimumab", "1234_other_ingredient"))

    ing_codes1 <- getDrugIngredientCodes(cdm, nameStyle = "{concept_id}_{concept_name}")
    expect_true(all(ing_codes1[[1]] == c(10L, 13L)))
    expect_identical(names(ing_codes1),  c("10_adalimumab", "19_other_ingredient"))

    ing_codes2 <- getDrugIngredientCodes(cdm, name = "Adalimumab")
    expect_true(all(ing_codes2[[1]] == c(10L, 13L)))
    expect_true(names(ing_codes2) == "1234_adalimumab")

    ing_codes2 <- getDrugIngredientCodes(cdm, name = 10)
    expect_true(all(ing_codes2[[1]] == c(10L, 13L)))
    expect_true(names(ing_codes2) == "1234_adalimumab")

    ing_codes3 <- getDrugIngredientCodes(cdm,
                                         name = "Adalimumab",
                                         doseForm = "injectable foam")
    expect_true(all(ing_codes3[[1]] == c(13L)))

    ing_codes4 <- getDrugIngredientCodes(cdm,
                                         name = "Adalimumab",
                                         doseForm = "injection")
    expect_true(all(ing_codes4[[1]] == c(10L)))

    ing_codes5 <- getDrugIngredientCodes(cdm,
                                         name = "Adalimumab",
                                         doseForm = "injection",
                                         type = "codelist_with_details")
    expect_true(!is.null(ing_codes5[[1]]$concept_name))
    expect_true(inherits(ing_codes5, "codelist_with_details"))

    # limiting on ingredients
    ing_codes_all <- getDrugIngredientCodes(cdm,
                                            ingredientRange = c(1, Inf))
    ing_codes_mono <- getDrugIngredientCodes(cdm,
                                             ingredientRange = c(1, 1))
    ing_codes_comb <- getDrugIngredientCodes(cdm,
                                             ingredientRange = c(2, Inf))

    expect_equal(ing_codes_all, ing_codes0)
    expect_true(all(c(10L) %in% ing_codes_mono$`1234_adalimumab`))
    expect_null(ing_codes_mono$`1234_other_ingredient`)
    expect_true(all(c(13L) %in% ing_codes_comb$`1234_adalimumab`))
    expect_true(all(c(13L) %in% ing_codes_comb$`1234_other_ingredient`))

    expect_error(getDrugIngredientCodes(cdm,
                                        ingredientRange = c(3, 2)))
    expect_error(getDrugIngredientCodes(cdm,
                                        ingredientRange = c(3)))
    expect_error(getDrugIngredientCodes(cdm,
                                        ingredientRange = c("a", "b")))
    expect_error(getDrugIngredientCodes(cdm,
                                        ingredientRange = c(-1, 25)))


    expect_warning(empty_res_1 <- getDrugIngredientCodes(cdm,
                                                         name = "not_a_drug_name",
                                                         type = "codelist"))
    expect_no_error(omopgenerics::newCodelist(empty_res_1))
    expect_warning(empty_res_2 <- getDrugIngredientCodes(cdm,
                                                         name = "not_a_drug_name",
                                                         type = "codelist_with_details"))
    expect_no_error(omopgenerics::newCodelistWithDetails(empty_res_2))

    expect_warning(getDrugIngredientCodes(cdm,
                                          name = c("Adalimumab", "not_a_drug_name")))
    expect_warning(getDrugIngredientCodes(cdm,
                                          name = c("Adalimumab",
                                                   "xxxxx",
                                                   "zzzzz")))

    # simple concept expression
    expect_true(nrow(getDrugIngredientCodes(cdm,
                                            name = "adalimumab",
                                            type = "concept_set_expression")[[1]]) == 1)

    # by adding criteria, we will now have specified codes
    expect_true(nrow(getDrugIngredientCodes(cdm,
                                            name = "adalimumab",
                                            type = "concept_set_expression",
                                            ingredientRange = c(1,4))[[1]]) == 2)
    expect_true(getDrugIngredientCodes(cdm,
                                       name = "adalimumab",
                                       type = "concept_set_expression",
                                       ingredientRange = c(1, 1))[[1]] |>
                  dplyr::pull("concept_id") == 10L)
    expect_true(getDrugIngredientCodes(cdm,
                                       name = "adalimumab",
                                       type = "concept_set_expression",
                                       ingredientRange = c(2, 2))[[1]] |>
                  dplyr::pull("concept_id") == 13L)
    expect_warning(
      expect_warning(
      # no concepts with 3 ingredients
      getDrugIngredientCodes(cdm,
                             name = "adalimumab",
                             type = "concept_set_expression",
                             ingredientRange = c(3, 3))
      )
    )

    # options not yet supported for concept set expression
    expect_error(getDrugIngredientCodes(cdm,
                                        name = "adalimumab",
                                        type = "concept_set_expression",
                                        doseUnit = c("mm")))
    expect_error(getDrugIngredientCodes(cdm,
                                        name = "adalimumab",
                                        type = "concept_set_expression",
                                        doseForm = "zzz"))
    expect_error(getDrugIngredientCodes(cdm,
                                        name = "adalimumab",
                                        type = "concept_set_expression",
                                        routeCategory = "xxx"))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})

test_that("no duplicate names example 1",{
  skip_on_cran()
  person <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 0L,
    year_of_birth = 1990L,
    race_concept_id = 0L,
    ethnicity_concept_id = 0L
  )
  observationPeriod <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2024-12-31"),
    period_type_concept_id = 0L
  )
  concept <- data.frame(
    concept_id = 1:19L,
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
    valid_start_date = as.Date(NA),
    valid_end_date = as.Date(NA),
    invalid_reason = NA_character_
  )
  conceptAncestor <- dplyr::bind_rows(
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 1L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 3L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 2L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 3L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 4L,
      min_levels_of_separation = 2L,
      max_levels_of_separation = 2L
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 5L,
      min_levels_of_separation = 2L,
      max_levels_of_separation = 2L
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 4L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 5L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 10L,
      descendant_concept_id = 10L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 10L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 12L,
      descendant_concept_id = 12L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 12L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 19L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
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
    dplyr::mutate(language_concept_id  = NA_integer_)

  vocabulary <- dplyr::bind_rows(
    data.frame(
      vocabulary_id = "SNOMED",
      vocabulary_name = "SNOMED",
      vocabulary_reference = "1",
      vocabulary_version = "1",
      vocabulary_concept_id = 1L
    ),
    data.frame(
      vocabulary_id = "None",
      vocabulary_name = "OMOP Standardized Vocabularies",
      vocabulary_reference = "Omop generated",
      vocabulary_version = "v5.0 22-JUN-22",
      vocabulary_concept_id = 44819096L
    )
  )

  drugStrength <- dplyr::bind_rows(
    data.frame(
      drug_concept_id = 10L,
      ingredient_concept_id = 10L,
      amount_value = NA_real_,
      amount_unit_concept_id = 8576L,
      numerator_value = 0.010,
      numerator_unit_concept_id = 8576L,
      denominator_value = 0.5,
      denominator_unit_concept_id = 8587L,
      box_size = NA_integer_,
      valid_start_date = as.Date(NA),
      valid_end_date = as.Date(NA)
    )
  )

  cdmSource <- dplyr::as_tibble(
    data.frame(
      cdm_source_name  = "mock",
      cdm_source_abbreviation = NA_character_,
      cdm_holder = NA_character_,
      source_description = NA_character_,
      source_documentation_reference = NA_character_,
      cdm_etl_reference = NA_character_,
      source_release_date = as.Date(NA),
      cdm_release_date = as.Date(NA),
      cdm_version = "5.3",
      vocabulary_version  = NA_character_
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
    dplyr::mutate(valid_start_date = as.Date(NA),
                  valid_end_date = as.Date(NA),
                  invalid_reason = NA_character_)

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
    person_id = 1L, gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observationPeriod <- dplyr::tibble(
    observation_period_id = 1L, person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2024-12-31"),
    period_type_concept_id = 0L
  )
  concept <- data.frame(
    concept_id = 1:19L,
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
    valid_start_date = as.Date(NA),
    valid_end_date = as.Date(NA),
    invalid_reason = NA_character_
  )
  conceptAncestor <- dplyr::bind_rows(
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 1L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 3L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 2L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 3L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 4L,
      min_levels_of_separation = 2L,
      max_levels_of_separation = 2L
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 5L,
      min_levels_of_separation = 2L,
      max_levels_of_separation = 2L
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 4L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 5L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 10L,
      descendant_concept_id = 10L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 10L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 12L,
      descendant_concept_id = 12L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 12L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 19L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
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
    dplyr::mutate(language_concept_id  = NA_integer_)

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
    dplyr::mutate(valid_start_date = as.Date(NA),
                  valid_end_date = as.Date(NA),
                  invalid_reason = NA_character_)

  vocabulary <- dplyr::bind_rows(
    data.frame(
      vocabulary_id = "SNOMED",
      vocabulary_name = "SNOMED",
      vocabulary_reference = "1",
      vocabulary_version = "1",
      vocabulary_concept_id = 1L
    ),
    data.frame(
      vocabulary_id = "None",
      vocabulary_name = "OMOP Standardized Vocabularies",
      vocabulary_reference = "Omop generated",
      vocabulary_version = "v5.0 22-JUN-22",
      vocabulary_concept_id = 44819096L
    )
  )

  drugStrength <- dplyr::bind_rows(
    data.frame(
      drug_concept_id = 10L,
      ingredient_concept_id = 10L,
      amount_value = NA_real_,
      amount_unit_concept_id = 8576L,
      numerator_value = 0.010,
      numerator_unit_concept_id = 8576L,
      denominator_value = 0.5,
      denominator_unit_concept_id = 8587L,
      box_size = NA_integer_,
      valid_start_date = as.Date(NA),
      valid_end_date = as.Date(NA)
    )
  )

  cdmSource <- dplyr::as_tibble(
    data.frame(
      cdm_source_name  = "mock",
      cdm_source_abbreviation = NA_character_,
      cdm_holder = NA_character_,
      source_description = NA_character_,
      source_documentation_reference = NA_character_,
      cdm_etl_reference = NA_character_,
      source_release_date = as.Date(NA),
      cdm_release_date = as.Date(NA),
      cdm_version = "5.3",
      vocabulary_version  = NA_character_
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
