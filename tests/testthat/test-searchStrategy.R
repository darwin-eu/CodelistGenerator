test_that("searchStrategy works", {
  cdm <- mockVocabRef()
  x <- getCandidateCodes(cdm, keywords = "a")
  expect_no_error(ss <- searchStrategy(x))
  expect_equal(attr(x, "search_strategy"), ss)

  # expect_identical(
  #   searchStrategy(x),
  #   dplyr::tibble(
  #     "vocabulary_version" = "v5.0 22-JUN-22",
  #     "keywords" = "Arthritis, Knee",
  #     "exclude"  = "Childhood asthma",
  #     "domains"  = "Condition, Observation",
  #     "standard_concept" = "Standard",
  #     "search_in_synonyms" = TRUE,
  #     "search_non_standard" = TRUE,
  #     "include_descendants" = TRUE,
  #     "include_ancestor" = TRUE
  #   )
  # )
  expect_identical(
    searchStrategy(x),
    attr(x, "search_strategy")
  )

  # Not a candidate_codes
  expect_error(searchStrategy(list("a" = 1L)))
  expect_error(ss <- searchStrategy(list("a")))
})
