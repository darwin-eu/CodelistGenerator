test_that("tests with db", {

if(Sys.getenv("darwinDbDatabaseServer")!=""){
db <-DBI::dbConnect(odbc::odbc(),
                      Driver   = "ODBC Driver 11 for SQL Server",
                      Server   = Sys.getenv("darwinDbDatabaseServer"),
                      Database = Sys.getenv("darwinDbDatabase"),
                      UID      = Sys.getenv("darwinDbUser"),
                      PWD      = Sys.getenv("darwinDbPassword"),
                      Port     = Sys.getenv("darwinDbDatabasePort"))

cdm <- CDMConnector::cdm_from_con(db,
                                  cdm_schema = Sys.getenv("darwinDbCdmSchema"),
                                  cdm_tables = tidyselect::all_of(c("concept",
                                                                    "concept_relationship",
                                                                    "concept_ancestor",
                                                                    "concept_synonym",
                                                                    "vocabulary")))
codes<-getCandidateCodes(cdm=cdm,
    keywords = "Musculoskeletal disorder",
    domains = "Condition"
  )
expect_true(nrow(codes)>1)

dbDisconnect(db)

}

})
