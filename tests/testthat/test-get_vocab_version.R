test_that("multiplication works", {
  untar(xzfile(system.file("sqlite", "cdm.tar.xz",
                           package = "Eunomia"),
               open = "rb"),
    exdir = tempdir()
  )
  db <- DBI::dbConnect(RSQLite::SQLite(), paste0(tempdir(), "\\cdm.sqlite"))
  vocab_version <- get_vocab_version(
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true(length(vocab_version) == 1)
})
