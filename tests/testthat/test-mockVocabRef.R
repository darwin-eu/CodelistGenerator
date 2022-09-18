test_that("mock vocab db", {
  cdm_db <- mockVocabRef("database")
  cdm_arrow <- mockVocabRef("arrow")
  cdm_data_frame <- mockVocabRef("data_frame")

  concept_from_db<-cdm_db$concept %>% dplyr::collect()
  concept_from_arrow<-cdm_arrow$concept %>% dplyr::collect()
  concept_from_df<-cdm_data_frame$concept %>% dplyr::collect()

  expect_true(dplyr::all_equal(concept_from_db,concept_from_arrow,concept_from_df))

})
