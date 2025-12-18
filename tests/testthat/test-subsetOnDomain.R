test_that("subset on domain", {

  backends <- c("database", "data_frame")

  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backends[[i]])
    ing_codes <- getDrugIngredientCodes(cdm)

    # all will have been empty and dropped
    expect_warning(ing_codes_sub <- subsetOnDomain(ing_codes, cdm, "condition"))
    expect_true(length(ing_codes_sub) == 0)
    expect_true(inherits(ing_codes_sub, "codelist"))

    ing_codes_sub2 <- subsetOnDomain(ing_codes,  cdm, "drug")
    expect_identical(ing_codes, ing_codes_sub2)
    expect_true(inherits(ing_codes_sub2, "codelist"))

    mixed_codelist <- omopgenerics::newCodelist(list("mixed_codes" = c(1L, # condition
                           9L,  # observation
                           10L))) # drug
    mixed_codelist1 <- subsetOnDomain(mixed_codelist,  cdm, "condition")
    expect_true(mixed_codelist1[[1]] == 1L)
    expect_true(length(mixed_codelist1[[1]]) == 1)
    expect_true(inherits(mixed_codelist1, "codelist"))

    mixed_codelist2 <- subsetOnDomain(mixed_codelist,  cdm,
                                      c("condition", "drug"))
    expect_true(all(sort(mixed_codelist2[[1]]) == c(1L, 10L)))
    expect_true(length(mixed_codelist2[[1]]) == 2)

    mixed_codelist3 <- subsetOnDomain(mixed_codelist,  cdm,
                                      c("condition", "drug", "observation"))
    expect_true(all(sort(mixed_codelist3[[1]]) == c(1L, 9L, 10L)))
    expect_true(length(mixed_codelist3[[1]]) == 3)

    mixed_codelist4 <- subsetOnDomain(omopgenerics::newCodelist(list("codes" = c(3L,4L))),  cdm,
                                      c("condition", "drug", "observation"))
    expect_true(inherits(mixed_codelist4, "codelist"))
    mixed_codelist5 <- subsetOnDomain(getDrugIngredientCodes(cdm, type = "codelist_with_details"),
                                      cdm, "drug")
    expect_true(inherits(mixed_codelist5,"codelist_with_details"))
    expect_true(!"codelist" %in% class(mixed_codelist5))

    # Check negate argument
    all_concepts <- omopgenerics::newCodelist(list("a" = cdm$concept |> dplyr::pull("concept_id")))
    codes1 <- subsetOnDomain(all_concepts, cdm, domain = c("Drug","Observation", "Unit"))
    codes2 <- subsetOnDomain(all_concepts, cdm, domain = "Condition", negate = TRUE)

    expect_identical(codes1, codes2)
    expect_identical(cdm$concept |>
      dplyr::filter(concept_id %in% codes2$a) |>
      dplyr::pull("domain_id") |>
      unique() |>
      sort(na.last = TRUE), c("Drug", "Observation", "Unit"))

    # Check if domain = NA
    cdm$concept <- cdm$concept |>
      dplyr::mutate(domain_id = dplyr::if_else(concept_id == 1, NA, domain_id)) |>
      dplyr::compute(name = "concept")
    expect_warning(
    codes1 <- subsetOnDomain(omopgenerics::newCodelist(list("a" = 1L)), cdm,
                             domain = c("Condition"))
    )
    expect_true(length(codes1) == 0)

    # check empty codelists
    expect_warning(
    x <- subsetOnDomain(omopgenerics::emptyCodelist(), cdm, domain = "Condition")
    )
    expect_true(inherits(x, "codelist"))

    expect_warning(
      x <- subsetOnDomain(omopgenerics::emptyCodelistWithDetails(), cdm,
                          domain = "Condition"))
    expect_true(inherits(x, "codelist_with_details"))
    expect_true(!"codelist" %in% class(x))

    # expected errors
    expect_error(subsetOnDomain(list("a" = 1L),  cdm, "condition"))
    expect_error(subsetOnDomain(ing_codes,  "a", "condition"))
    expect_error(subsetOnDomain(ing_codes,  cdm, 1234))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }

})
