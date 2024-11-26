test_that("tests with mock db", {
    # mock db
    cdm <- mockVocabRef("database")

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      domains = "Condition",
      includeDescendants = FALSE
    )

   orphan_codes <- summariseOrphanCodes(x = list("msk" = codes$concept_id),
                    cdm = cdm)

   # we should pick up knee osteoarthritis from our achilles tables
   expect_true(all(stringr::str_detect(orphan_codes |>
     dplyr::pull("variable_level"), c("4", "5"))))
   expect_equal(orphan_codes |>
                       dplyr::pull("estimate_value"),
               c("400", "200"))
   settings <- omopgenerics::settings(orphan_codes)
   expect_true(all(settings$result_type == "orphan_code_use"))

   # with phoebe present
   cdm <- omopgenerics::insertTable(cdm,
                             name = "concept_recommended",
                             table = data.frame(concept_id_1 = 1,
                                                concept_id_2 = 9,
                                                relationship_id = "from phoebe"),
   overwrite = TRUE,
   temporary = FALSE)

   orphan_codes <- summariseOrphanCodes(x = list("msk" = codes$concept_id),
                                        cdm = cdm)
   expect_true(9 %in% orphan_codes$variable_level)


   cdm <- omopgenerics::insertTable(cdm,
                                    name = "concept_recommended",
                                    table = data.frame(concept_id_1 = 8,
                                                       concept_id_2 = 9,
                                                       relationship_id = "from phoebe"),
                                    overwrite = TRUE,
                                    temporary = FALSE)
   orphan_codes <- summariseOrphanCodes(x = list("msk" = codes$concept_id),
                                        cdm = cdm)
   expect_true(!9 %in% orphan_codes$variable_level)

   #expected error
   expect_error(summariseOrphanCodes(x = "a", cdm = cdm))
   expect_error(summariseOrphanCodes(x = list("msk" = codes$concept_id),
                                     cdm = "a"))

   # requires achilles
   cdm$achilles_results <- NULL
   expect_error(summariseOrphanCodes(x = list("msk" = codes$concept_id),
                                      cdm = cdm))

   CDMConnector::cdm_disconnect(cdm)

})
