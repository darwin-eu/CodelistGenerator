test_that("tests with mock db", {

    # mock db
    cdm <- mockVocabRef("database")

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      domains = "Condition",
      includeDescendants = FALSE
    )

   orphan_codes <- findOrphanCodes(x = list("msk" = codes$concept_id),
                    cdm = cdm,
                    domains = "Condition",
                    standardConcept = "Standard",
                    searchInSynonyms = FALSE,
                    searchNonStandard = FALSE,
                    includeDescendants = TRUE,
                    includeAncestor = FALSE)

   # we should pick up knee osteoarthritis from our achilles tables
   expect_equal(orphan_codes %>%
     dplyr::pull("group_level"), c("4", "5"))
   expect_equal(orphan_codes %>%
                       dplyr::pull("estimate_value"),
               c("400", "200"))

   orphan_codes <- findOrphanCodes(x = list("msk" = codes$concept_id),
                                   cdm = cdm,
                                   domains = "Condition",
                                   standardConcept = "Standard",
                                   searchInSynonyms = FALSE,
                                   searchNonStandard = FALSE,
                                   includeDescendants = FALSE,
                                   includeAncestor = FALSE)
   # we will not find records now we're not looking in descendants
   expect_true(nrow(orphan_codes) == 0)


   # we shouldn't have our original codes
   expect_true(nrow(findOrphanCodes(x = list("knee_oa" = 4),
                   cdm = cdm,
                   domains = "Condition",
                   standardConcept = "Standard",
                   searchInSynonyms = FALSE,
                   searchNonStandard = FALSE,
                   includeDescendants = FALSE,
                   includeAncestor = FALSE)) == 0)

   # min cell count
   orphan_codes <- findOrphanCodes(x = list("msk" = codes$concept_id),
                                   cdm = cdm,
                                   domains = "Condition",
                                   standardConcept = "Standard",
                                   searchInSynonyms = FALSE,
                                   searchNonStandard = FALSE,
                                   includeDescendants = TRUE,
                                   includeAncestor = FALSE,
                                   minCellCount = 150)
   expect_equal(orphan_codes %>%
                 dplyr::pull("group_level"), c("4", "5"))
   # expect_true(is.na(orphan_codes %>%
   #   dplyr::pull("estimate_value")))

   CDMConnector::cdm_disconnect(cdm)

})
