test_that("mock vocab db", {
  cdmDb <- mockVocabRef("database")
  cdmDF <- mockVocabRef("data_frame")
  cdmArrow <- mockVocabRef("arrow")

  conceptFromDb <- cdmDb$concept %>% dplyr::collect()
  conceptFromDf <- cdmDF$concept %>% dplyr::collect()
  conceptFromArrow <- cdmArrow$concept %>% dplyr::collect()

  expect_equal(conceptFromDb,
                    conceptFromDf,
                    ignore_attr = TRUE)
  expect_equal(conceptFromDb,
               conceptFromArrow,
               ignore_attr = TRUE)

  DBI::dbDisconnect(attr(cdmDb, "dbcon"), shutdown = TRUE)
})
