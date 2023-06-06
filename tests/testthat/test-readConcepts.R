test_that("test inputs", {
  testthat::skip_if(Sys.getenv("CDM5_REDSHIFT_DBNAME") == "")


  db <-  DBI::dbConnect(RPostgres::Redshift(),
                        dbname   = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
                        host     = Sys.getenv("CDM5_REDSHIFT_HOST"),
                        port     = Sys.getenv("CDM5_REDSHIFT_PORT"),
                        user     = Sys.getenv("CDM5_REDSHIFT_USER"),
                        password = Sys.getenv("CDM5_REDSHIFT_PASSWORD"))


  cdm <- CDMConnector::cdm_from_con(con = db,
                                    cdm_schema = "cdmv531",
                                    write_schema = "public")
  expect_error(readConceptList())
  expect_error(readConceptList(cdm = cdm))
  expect_error(readConceptList(cdm = cdm, path = 1))
  expect_error(readConceptList(cdm = cdm, path = "not/a/path"))
  x <- readConceptList(
    cdm = cdm, path =  system.file(package = "CodelistGenerator", "concepts")
  )
  expect_true(typeof(x) == "list")
  expect_true(all(names(x) %in% c("influenza", "acetaminophen")))
  expect_true(x$influenza == 4266367)
  expect_true(all(1125315 %in% x$acetaminophen))
  expect_true(length(x$acetaminophen) > 1)
})
