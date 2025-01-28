test_that("test inputs - mock", {
  backends <- c("database", "data_frame")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])


    # expected errors
  expect_error(codesFromConceptSet())
  expect_error(codesFromConceptSet(cdm = cdm))
  expect_error(codesFromConceptSet(cdm = cdm, path = 1))
  expect_error(codesFromConceptSet(cdm = cdm, path = "not/a/path"))

  expect_error(codesFromCohort())
  expect_error(codesFromCohort(cdm = cdm))
  expect_error(codesFromCohort(cdm = cdm, path = 1))
  expect_error(codesFromCohort(cdm = cdm, path = "not/a/path"))


  # codesFromCohort won´t work with concept sets
  expect_error( codesFromCohort(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "concepts_for_mock")
  ))
  # codesFromConceptSet won´t work with cohorts
  expect_error(codesFromConceptSet(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "cohorts_for_mock")
  ))



  # we currently don´t support the use of mapped in a concept set
  expect_error(codesFromConceptSet(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "concepts_for_mock_with_mapped")
  ))
  expect_error(codesFromCohort(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "cohorts_for_mock_with_mapped")
  ))

  # working concept set example with mock
  x <- codesFromConceptSet(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "concepts_for_mock")
  )
  expect_true(x$oa_no_desc == "3")
  expect_true(all(c("3", "4", "5") %in% x$oa_desc))

  x <- codesFromConceptSet(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "concepts_for_mock"),
    type = "codelist_with_details"
  )
  expect_true(inherits(x, "codelist_with_details"))

  x <- codesFromConceptSet(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "concepts_for_mock"),
    type = "concept_set_expression"
  )
  expect_true(inherits(x, "concept_set_expression") |
                inherits(x, "conceptSetExpression"))

  x <- codesFromConceptSet(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "concepts_for_mock_with_exclude")
  )
  expect_true(all(c("3", "5") %in% x$oa_with_excluded))
  expect_true(!c("4") %in% x$oa_with_excluded)

  # withDetails
  x <- codesFromConceptSet(
    cdm = cdm,
    path =  system.file(package = "CodelistGenerator",
                                   "concepts_for_mock"),
    type = "codelist_with_details"
  )
  expect_true("Arthritis" %in% x$oa_no_desc$concept_name)
  expect_true(3 %in% x$oa_no_desc$concept_id)


  # working cohort set example with mock
  x <- codesFromCohort(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "cohorts_for_mock")
  )
  expect_true(all(c("3", "4", "5") %in% x[["OA"]]))
  expect_true(x[["OA no descendants"]] == 3)

  x <- codesFromCohort(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "cohorts_for_mock_with_exclude")
  )
  expect_true(all(c("3", "5") %in% x[["OA"]]))
  expect_true(!c("4") %in% x[["OA"]])

  x <- codesFromCohort(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "cohorts_for_mock_with_exclude"),
    type = "codelist_with_details"
  )
  expect_true(inherits(x, "codelist_with_details"))

  x <- codesFromCohort(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "cohorts_for_mock_with_exclude"),
    type = "concept_set_expression"
  )
  expect_true(inherits(x, "concept_set_expression") |
              inherits(x, "conceptSetExpression"))

  # we´ll get an error if we have the same name concept set in multiple cohorts,
  # but with different definitions
  expect_error(codesFromCohort(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "cohorts_for_mock_dups")
  ))

  if (backends[[i]] == "database") {
  CDMConnector::cdmDisconnect(cdm)
    }

  }

})

test_that("test inputs - redshift", {
  testthat::skip_if(Sys.getenv("CDM5_REDSHIFT_DBNAME") == "")
  testthat::skip_if_offline()

  db <-  DBI::dbConnect(RPostgres::Redshift(),
                        dbname   = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
                        host     = Sys.getenv("CDM5_REDSHIFT_HOST"),
                        port     = Sys.getenv("CDM5_REDSHIFT_PORT"),
                        user     = Sys.getenv("CDM5_REDSHIFT_USER"),
                        password = Sys.getenv("CDM5_REDSHIFT_PASSWORD"))


  cdm <- CDMConnector::cdmFromCon(con = db,
                                    cdmSchema = "cdmv531",
                                    writeSchema = "public")

  # working example
  x <- codesFromConceptSet(
    cdm = cdm, path =  system.file(package = "CodelistGenerator", "concepts")
  )
  expect_true(typeof(x) == "list")
  expect_true(all(names(x) %in% c("influenza", "acetaminophen")))
  expect_true(x$influenza == 4266367)
  expect_true(all(1125315 %in% x$acetaminophen))
  expect_true(length(x$acetaminophen) > 1)

  x <- codesFromConceptSet(
    cdm = cdm,
    path =  system.file(package = "CodelistGenerator",
                        "concepts"),
    type = "codelist_with_details"
  )
  expect_true("Influenza" %in% x$influenza$concept_name)

  x <- codesFromCohort(
    cdm = cdm,
    path =  system.file(package = "CodelistGenerator",
                        "cohorts"),
    type = "codelist_with_details"
  )
  expect_true("Influenza" %in% x$influenza$concept_name)

  # check is sorted alphabetically
  x <- codesFromCohort(
    cdm = cdm,
    path =  system.file(package = "CodelistGenerator",
                        "cohorts2"),
    type = "codelist_with_details"
  )

  expect_equal(sort(x[[1]]$concept_name), x[[1]]$concept_name)
  expect_equal(sort(x[[2]]$concept_name), x[[2]]$concept_name)
  expect_equal(sort(x[[3]]$concept_name), x[[3]]$concept_name)

  CDMConnector::cdmDisconnect(cdm)
})



