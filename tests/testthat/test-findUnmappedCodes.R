test_that("achilles code use", {

  testthat::skip_if(Sys.getenv("CDM5_REDSHIFT_DBNAME") == "")
  testthat::skip_if_offline()

  db <-  DBI::dbConnect(RPostgres::Redshift(),
                        dbname   = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
                        host     = Sys.getenv("CDM5_REDSHIFT_HOST"),
                        port     = Sys.getenv("CDM5_REDSHIFT_PORT"),
                        user     = Sys.getenv("CDM5_REDSHIFT_USER"),
                        password = Sys.getenv("CDM5_REDSHIFT_PASSWORD"))

  cdm <- CDMConnector::cdm_from_con(con = db,
                                    cdm_schema = Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA"),
                                    write_schema = Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA"))


  x <- "Nonallopathic lesions"

  candidateCodes <- getCandidateCodes(
    cdm = cdm,
    keywords = "Nonallopathic lesions",
    domains = "Condition",
    standardConcept = "Non-standard",
    searchInSynonyms = FALSE,
    searchNonStandard = FALSE,
    includeDescendants = FALSE,
    includeAncestor = FALSE)

  # source codes used in the database
  dbCandidateCodes <- intersect(unmappedSourceCodesInUse(cdm = cdm),
                                candidateCodes$concept_id)


  a<- candidateCodes

  a<-cdm$condition_occurrence %>%
    dplyr::filter(condition_concept_id == 0) %>%
    dplyr::collect()


  CDMConnector::cdm_disconnect(cdm)
})
