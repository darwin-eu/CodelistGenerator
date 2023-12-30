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

   # no records with codes in the database, so we shouldn't get any orphan codes
   expect_true(nrow(orphan_codes$msk) == 0)

   # if we add achilles counts and rerun we should now get some orphan concepts
   cdm$achilles_analysis <- dplyr::tibble()
   cdm$achilles_results_dist <- dplyr::tibble()
   cdm$achilles_results <- dplyr::tibble(stratum_1 = c(2, 3),
                   stratum_2 = NA,
                   stratum_3 = NA,
                   analysis_id = 401,
                   count_value = 10)
   orphan_codes <- findOrphanCodes(x = list("msk" = codes$concept_id),
                                   cdm = cdm,
                                   domains = "Condition",
                                   standardConcept = "Standard",
                                   searchInSynonyms = FALSE,
                                   searchNonStandard = FALSE,
                                   includeDescendants = TRUE,
                                   includeAncestor = FALSE)

   # we shouldn't have our original codes
   expect_true(!1 %in% orphan_codes[["msk"]]$concept_id)
   # we should have our codes which have an achilles record count
   expect_true(all(orphan_codes[["msk"]]$concept_id %in% c(2,3)))
   # but we should not have codes with no achilles record count
   expect_true(!all(orphan_codes[["msk"]]$concept_id %in% c(4,5)))

 DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("sql server with achilles", {

  testthat::skip_if(Sys.getenv("CDM5_SQL_SERVER_SERVER") == "")
  testthat::skip_if(Sys.getenv("SQL_SERVER_DRIVER") == "")
  testthat::skip_if(packageVersion("CDMConnector") <= "1.2.0")

  db <- DBI::dbConnect(odbc::odbc(),
                       Driver   = Sys.getenv("SQL_SERVER_DRIVER"),
                       Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                       Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
                       UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
                       PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
                       TrustServerCertificate="yes",
                       Port     = Sys.getenv("CDM5_SQL_SERVER_PORT"))
  cdm <- CDMConnector::cdm_from_con(db,
                                    cdm_schema = c("CDMV54", "dbo"),
                                    write_schema = c("ohdsi", "dbo"))

  asthma_codes <- getCandidateCodes(
    cdm = cdm,
    keywords = "asthma",
    domains = c("Condition"),
    includeDescendants = FALSE
  )
  expect_no_error(orphan_codes <- findOrphanCodes(x = list("asthma" = asthma_codes$concept_id),
                                                  cdm = cdm))
  expect_no_error(all(orphan_codes[["asthma"]]$domain_id == "Condition"))

  CDMConnector::cdm_disconnect(cdm)
})
