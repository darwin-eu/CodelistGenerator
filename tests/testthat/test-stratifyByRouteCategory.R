test_that("stratify by route works", {
  backends <- c("database", "data_frame")

  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backends[[i]])
    ing_codes <- getDrugIngredientCodes(cdm)

    ing_codes_str <- stratifyByRouteCategory(ing_codes, cdm)
    # all will are unclassified
    expect_true( all(stringr::str_detect(names(ing_codes_str),
                                        "unclassified")))

    # can also keep the original
    ing_codes_str_all <- stratifyByRouteCategory(ing_codes, cdm,
                                                 keepOriginal = TRUE)
    expect_true(length(ing_codes_str_all) == 4)


    # if concepts are not from the drug domain we should get empty codelist back
    oa <- getCandidateCodes(cdm = cdm, "osteoarthritis")
    oa_str <- stratifyByRouteCategory(list(oa = oa$concept_id),
                                      cdm, keepOriginal = FALSE)
    expect_true(length(oa_str)==0)

    oa_str <- stratifyByRouteCategory(omopgenerics::newCodelistWithDetails(list(oa = oa)),
                                      cdm, keepOriginal = FALSE)
    expect_true(length(oa_str)==0)

    # expected errors
    expect_error(stratifyByRouteCategory("a",  cdm))
    expect_error(stratifyByRouteCategory(ing_codes,  "a"))


    if (backends[[i]] == "database") {
      CDMConnector::cdm_disconnect(cdm)
    }
  }
})
