test_that("mock db", {
db<-generate_mock_db()
expect_true(inherits(db, "DBIConnection"))
})
