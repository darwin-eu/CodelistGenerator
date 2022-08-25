test_that("mock vocab db", {
  db <- mockVocab()
  expect_true(inherits(db, "DBIConnection"))
  DBI::dbDisconnect(db)
})
