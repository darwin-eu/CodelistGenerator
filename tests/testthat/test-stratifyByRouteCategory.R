test_that("stratify by route works", {
  backends <- c("database", "data_frame")

  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backends[[i]])
    ing_codes <- getDrugIngredientCodes(cdm)

    ing_codes_str <- stratifyByRouteCategory(ing_codes, cdm)

     # all will are unclassified
    expect_true( all(stringr::str_detect(names(ing_codes_str),
                                        "injectable")))

    # can also keep the original
    ing_codes_str_all <- stratifyByRouteCategory(ing_codes, cdm,
                                                 keepOriginal = TRUE)
    expect_true(length(ing_codes_str_all) == 4)


    # if concepts are not from the drug domain we should an unclassified codelist
    oa <- getCandidateCodes(cdm = cdm, "osteoarthritis")
    oa_str <- stratifyByRouteCategory(omopgenerics::newCodelist(list(oa = oa$concept_id)),
                                      cdm, keepOriginal = FALSE)
    expect_true(names(oa_str)=="oa_unclassified_route_category")

    oa_str <- stratifyByRouteCategory(omopgenerics::newCodelistWithDetails(list(oa = oa)),
                                      cdm, keepOriginal = FALSE)
    expect_true(names(oa_str)=="oa_unclassified_route_category")
    expect_true(inherits(oa_str, "codelist_with_details"))

    # expected errors
    expect_error(stratifyByRouteCategory(list("a" = 1L),  cdm))
    expect_error(stratifyByRouteCategory(ing_codes,  "a"))
    expect_error(stratifyByRouteCategory(ing_codes,  cdm, "a"))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})
