test_that("mock vocab db", {
  db <- generateMockVocabDb()
  expect_true(inherits(db, "DBIConnection"))
  DBI::dbDisconnect(db)
})
