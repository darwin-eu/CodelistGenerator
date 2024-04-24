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
                    cdm = cdm,
                    domains = "Condition",
                    standardConcept = "Standard",
                    searchInSynonyms = FALSE,
                    searchNonStandard = FALSE,
                    includeDescendants = TRUE,
                    includeAncestor = FALSE)

   # we should pick up knee osteoarthritis from our achilles tables
   expect_true(all(stringr::str_detect(orphan_codes %>%
     dplyr::pull("variable_level"), c("4", "5"))))
   expect_equal(orphan_codes %>%
                       dplyr::pull("estimate_value"),
               c("400", "200"))
   settings <- omopgenerics::settings(orphan_codes)
   expect_true(all(
     colnames(settings) ==
       c("result_id", "result_type", "package_name", "package_version", "search_domains",
         "search_standard_concept", "search_in_synonyms", "search_non_standard",
         "include_descendants", "include_ancestor")))
   expect_true(settings$search_domains == "Condition")
   expect_true(settings$result_type == "orphan_codes")

   orphan_codes <- summariseOrphanCodes(x = list("msk" = codes$concept_id),
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
   expect_true(nrow(summariseOrphanCodes(x = list("knee_oa" = 4),
                   cdm = cdm,
                   domains = "Condition",
                   standardConcept = "Standard",
                   searchInSynonyms = FALSE,
                   searchNonStandard = FALSE,
                   includeDescendants = FALSE,
                   includeAncestor = FALSE)) == 0)

   # min cell count
   orphan_codes <- summariseOrphanCodes(x = list("msk" = codes$concept_id),
                                   cdm = cdm,
                                   domains = "Condition",
                                   standardConcept = "Standard",
                                   searchInSynonyms = FALSE,
                                   searchNonStandard = FALSE,
                                   includeDescendants = TRUE,
                                   includeAncestor = FALSE)
   expect_true(all(is.na(
     orphan_codes %>%
       omopgenerics::suppress(minCellCount = 500) %>%
       dplyr::pull("estimate_value"))))

   expect_true(all(orphan_codes |> visOmopResults::additionalColumns() ==
                     c("standard_concept", "vocabulary_id", "relationship_id")))

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

  expect_no_error(oc <- summariseOrphanCodes(asthma_cs, cdm = cdm,
                  domains = c("condition", "observation")))
  expect_true(nrow(oc) > 0)
  settings <- omopgenerics::settings(oc)
  expect_true(all(colnames(settings) ==
                    c("result_id", "result_type", "package_name", "package_version", "search_domains",
                      "search_standard_concept", "search_in_synonyms", "search_non_standard", "include_descendants", "include_ancestor")))
  expect_true(settings$search_domains == "condition &&& observation")
  expect_true(settings$result_type == "orphan_codes")
  expect_true(all(oc |> visOmopResults::additionalColumns() ==
                    c("standard_concept", "vocabulary_id", "relationship_id")))

  # no codes
  asthma_cs <- omopgenerics::newCodelist(list("asthma" = c(317009, 4051466)))
  expect_no_error(oc <- summariseOrphanCodes(asthma_cs, cdm = cdm,
                                        domains = c("condition", "observation")))

  # one with codes, one without
  asthma_cs <- omopgenerics::newCodelist(list("asthma_1" = c(317009, 4051466),
                                              "asthma_2" = c(317009)))
  expect_no_error(oc <- summariseOrphanCodes(asthma_cs, cdm = cdm,
                                        domains = c("condition", "observation")))
  expect_true(nrow(oc) > 0)
})
