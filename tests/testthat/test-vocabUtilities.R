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

  descendants <- getDescendants(cdm=cdm,
                 concept_id = 1)
  expect_true(all(descendants$concept_id == c(1,2,3,4,5)))

  # expected errors
  expect_error(getVocabVersion(cdm="a"))
  expect_error(getVocabularies(cdm="a"))
  expect_error(getDomains(cdm="a"))
  expect_error(getconceptClassId(cdm="a"))
  expect_error(getDescendants(cdm="a"))

}
})
