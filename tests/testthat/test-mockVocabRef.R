test_that("mock vocab db", {
  cdmDb <- mockVocabRef("database")
  cdmArrow <- mockVocabRef("arrow")
  cdmDF <- mockVocabRef("data_frame")

  conceptFromDb <- cdmDb$concept %>% dplyr::collect()
  conceptFromArrow <- cdmArrow$concept %>% dplyr::collect()
  conceptFromDf <- cdmDF$concept %>% dplyr::collect()

  expect_true(dplyr::all_equal(conceptFromDb,
                               conceptFromArrow,
                               conceptFromDf))

  DBI::dbDisconnect(attr(cdmDb, "dbcon"), shutdown = TRUE)
})
