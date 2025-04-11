test_that("subset on route category", {

  backends <- c("database", "data_frame")

  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backends[[i]])
    ing_codes <- getDrugIngredientCodes(cdm)

    ing_codes_sub <- subsetOnRouteCategory(ing_codes, cdm, "oral")
    # all will have been empty and dropped
    expect_true(length(ing_codes_sub) == 0)

    ing_codes_sub <- subsetOnRouteCategory(ing_codes,  cdm, "oral")

    ing_codes_sub2 <- subsetOnRouteCategory(ing_codes,  cdm, "unclassified_route")
    expect_identical(ing_codes, ing_codes_sub2)

    # expected errors
    expect_error(subsetOnRouteCategory("a",  cdm, "oral"))
    expect_error(subsetOnRouteCategory(ing_codes,  "a", "oral"))
    expect_error(subsetOnRouteCategory(ing_codes,  cdm, 1234))

    # Check negate option
    cdm <- mockVocabRef()
    x <- list("codes" = c(20L,21L))

    topical1 <- subsetOnRouteCategory(x, cdm, routeCategory = "topical")
    topical2 <- subsetOnRouteCategory(x, cdm, routeCategory = "transmucosal_nasal", negate = TRUE)

    expect_equal(topical1$codes, topical2$codes)
    expect_true(cdm$concept |>
                  dplyr::filter(concept_id %in% topical2$codes) |>
                  dplyr::select("concept_id") |>
                  dplyr::left_join(
                    cdm$concept_relationship |>
                      dplyr::select("concept_id" = "concept_id_1",
                                    "concept_id_2", "relationship_id") |>
                      dplyr::filter(relationship_id == "RxNorm has dose form"),
                    by = "concept_id"
                  ) |>
                  dplyr::left_join(
                    doseFormToRoute |>
                      dplyr::rename("concept_id_2" = "dose_form_concept_id"),
                    by = "concept_id_2"
                  ) |>
                  dplyr::pull("route_category") |>
                  unique() == "topical")

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})

