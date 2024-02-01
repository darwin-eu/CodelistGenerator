test_that("mock vocab db", {
  cdmDb <- mockVocabRef("database")
  cdmDF <- mockVocabRef("data_frame")

  conceptFromDb <- cdmDb$concept %>% dplyr::collect()
  conceptFromDf <- cdmDF$concept %>% dplyr::collect()

  expect_equal(conceptFromDb,
                    conceptFromDf,
                    ignore_attr = TRUE)

  CDMConnector::cdm_disconnect(cdm = cdmDb)
  })
