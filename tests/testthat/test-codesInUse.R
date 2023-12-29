test_that("tests with mock db", {

  # mock db
  cdm <- mockVocabRef("database")

  codes <- getCandidateCodes(
    cdm = cdm,
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = FALSE
  )

 expect_message(restrictToCodesInUse(list("cs" = codes$concept_id),
                       cdm = cdm)) # no codes in db

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})


test_that("sql server with achilles", {

  testthat::skip_if(Sys.getenv("CDM5_SQL_SERVER_SERVER") == "")
  testthat::skip_if(Sys.getenv("SQL_SERVER_DRIVER") == "")

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
                                    achilles_schema = c("CDMV54", "dbo"),
                                    write_schema = c("ohdsi", "dbo"))

  asthma_codes <- getCandidateCodes(
    cdm = cdm,
    keywords = "asthma",
    domains = c("Condition"),
    includeDescendants = TRUE
  )
  asthma_cl <- list("cs" = asthma_codes$concept_id)

  asthma_codes_present <- restrictToCodesInUse(x = asthma_cl,
                                               cdm = cdm)

expect_equal(sort(asthma_codes_present[[1]]),
             sort(cdm$condition_occurrence %>%
   dplyr::filter(.data$condition_concept_id %in%
                   !!asthma_codes$concept_id) %>%
   dplyr::select("condition_concept_id") %>%
   dplyr::distinct() %>%
   dplyr::pull()))


  CDMConnector::cdm_disconnect(cdm)
})
