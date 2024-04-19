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
   expect_true(all(stringr::str_detect(orphan_codes %>%
     dplyr::pull("additional_level"), c("4", "5"))))
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
                                   minCellCount = 500)
   expect_true(all(stringr::str_detect(orphan_codes %>%
                                         dplyr::pull("additional_level"),
                                       c("4", "5"))))
   expect_true(all(is.na(orphan_codes %>%
     dplyr::pull("estimate_value"))))

   CDMConnector::cdm_disconnect(cdm)

})

test_that("tests with eunomia - no achilles", {

  skip_on_cran()
  skip_on_ci()
  db <- DBI::dbConnect(duckdb::duckdb(),
                       dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = "main",
    write_schema = "main"
  )

  asthma_cs <- omopgenerics::newCodelist(list("asthma" = 317009))

  expect_no_error(oc <- findOrphanCodes(asthma_cs, cdm = cdm,
                  domains = c("condition", "observation")))
  expect_true(nrow(oc) > 0)

  # no codes
  asthma_cs <- omopgenerics::newCodelist(list("asthma" = c(317009, 4051466)))
  expect_no_error(oc <- findOrphanCodes(asthma_cs, cdm = cdm,
                                        domains = c("condition", "observation")))

  # one with codes, one without
  asthma_cs <- omopgenerics::newCodelist(list("asthma_1" = c(317009, 4051466),
                                              "asthma_2" = c(317009)))
  expect_no_error(oc <- findOrphanCodes(asthma_cs, cdm = cdm,
                                        domains = c("condition", "observation")))

})
