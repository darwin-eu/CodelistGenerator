test_that("tests with mock db", {

  backends<- c("database", "arrow","data_frame")

  for(i in 1:length(backends)){
    # mock db
    cdm <- mockVocabRef(backends[[i]])

  version <- getVocabVersion(cdm=cdm)
  expect_true(length(version)==1)
  expect_true(is.character(version))

  vocabs <- getVocabularies(cdm=cdm)
  expect_true(length(vocabs)>=1)
  expect_true(is.character(vocabs))

  domains<-getDomains(cdm=cdm)
  expect_true(all(c("Condition", "Observation") %in% domains))
  expect_true(is.character(domains))

  concept_classes <- getconceptClassId(cdm=cdm)
  expect_true(is.character(concept_classes))

  concept_classes <- getconceptClassId(cdm=cdm,
                    domain = "Condition")
  expect_true(is.character(concept_classes))

}
})
