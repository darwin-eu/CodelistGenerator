test_that("mock vocab db", {
  db <- generate_mock_vocab_db()
  expect_true(inherits(db, "DBIConnection"))
  DBI::dbDisconnect(db)
})
