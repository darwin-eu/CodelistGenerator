test_that("addConcepts works", {
  cdm <- mockVocabRef(backend = "data_frame")

  # Test codelist
  codelist <- omopgenerics::newCodelist(list(
      "conditions" = c(1L),
      "drug" = c(10L),
      "other" = c(13L)))

  expect_no_error(codelist1 <- addConcepts(codelist,
                                          cdm,
                                          concepts = c(10L, 13L),
                                          codelistName = "conditions"))
  expect_identical(names(codelist1), names(codelist))
  expect_identical(class(codelist1), class(codelist))
  expect_identical(codelist1$conditions, c(1L, 10L, 13L))
  expect_identical(codelist1$drug, c(10L))
  expect_identical(codelist1$other, c(13L))

  # Test codelist with details
  codelist <- codelist |> asCodelistWithDetails(cdm)

  expect_no_error(codelist1 <- addConcepts(x = codelist,
                                           cdm,
                                           concepts = c(10L, 13L),
                                           codelistName = "conditions"))
  expect_identical(names(codelist1), names(codelist))
  expect_identical(class(codelist1), class(codelist))
  expect_identical(codelist1$conditions,
                   dplyr::tibble(
                     "concept_id" = c(1L, 10L, 13L),
                     "concept_name" = c("Musculoskeletal disorder", "Adalimumab", "Descendant drug"),
                     "domain_id" = c("Condition", "Drug", "Drug"),
                     "vocabulary_id" = c("SNOMED", "RxNorm", "RxNorm"),
                     "standard_concept" = rep("standard",3)
                   ))
  expect_identical(codelist1$drug,
                   dplyr::tibble(
                     "concept_id" = 10L,
                     "concept_name" = "Adalimumab",
                     "domain_id" = "Drug",
                     "vocabulary_id" = "RxNorm",
                     "standard_concept" = "standard"
                   ))


  # Test codelist + descendants + all concepts
  expect_no_error(codelist1 <- addConcepts(codelist,
                                           cdm,
                                           concepts = c(1L),
                                           codelistName = NULL))
  expect_identical(nrow(codelist1$conditions), 1L)
  expect_identical(nrow(codelist1$drug), 2L)
  expect_identical(nrow(codelist1$other), 2L)

  # Expected errors
  expect_error(addConcepts("hola", cdm, concepts = 1L))
  expect_error(addConcepts(list("a" = 1L), cdm, concepts = 1L))
  expect_error(addConcepts(codelist, "hola", concepts = 1L))
  expect_error(addConcepts(codelist, cdm, concepts = "hola"))
  expect_error(addConcepts(codelist, cdm, concepts = 1L, codelistName = "hola"))
})
