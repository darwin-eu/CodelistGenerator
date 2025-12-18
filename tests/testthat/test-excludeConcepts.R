test_that("excludeConcepts works", {
  cdm <- mockVocabRef(backend = "data_frame")

  # Test codelist
  codelist <- omopgenerics::newCodelist(list(
    "conditions" = c(1L),
    "drug" = c(10L, 1L, 13L, 2L, 3L),
    "other" = c(13L, 10L)))

  expect_no_error(codelist1 <- excludeConcepts(codelist,
                                               cdm,
                                               concepts = c(10L, 13L),
                                               codelistName = "drug"))
  expect_identical(names(codelist1), names(codelist))
  expect_identical(class(codelist1), class(codelist))
  expect_identical(codelist1$conditions, c(1L))
  expect_identical(codelist1$drug, c(1L, 2L, 3L))
  expect_identical(sort(codelist1$other), sort(c(13L, 10L)))

  # Test codelist with details
  codelist <- codelist |> asCodelistWithDetails(cdm)

  expect_no_error(codelist1 <- excludeConcepts(x = codelist,
                                               cdm,
                                               concepts = c(10L, 13L),
                                               codelistName = "drug"))
  expect_identical(names(codelist1), names(codelist))
  expect_identical(class(codelist1), class(codelist))
  expect_identical(codelist1$drug,
                   dplyr::tibble(
                     "concept_id" = c(1L, 2L, 3L),
                     "concept_name" = c("Musculoskeletal disorder", "Osteoarthrosis", "Arthritis"),
                     "domain_id" = rep("Condition",3),
                     "vocabulary_id" = rep("SNOMED", 3),
                     "standard_concept" = rep("standard",3)
                   ))
  expect_identical(codelist1$other,
                   dplyr::tibble(
                     "concept_id" = c(10L, 13L),
                     "concept_name" = c("Adalimumab","Descendant drug"),
                     "domain_id" = c("Drug", "Drug"),
                     "vocabulary_id" = c("RxNorm", "RxNorm"),
                     "standard_concept" = rep("standard",2)
                   ))


  # Test codelist + descendants + all concepts
  expect_warning(codelist1 <- excludeConcepts(codelist,
                                               cdm,
                                               concepts = c(1L),
                                               codelistName = NULL))
  expect_true(!"conditions" %in% names(codelist1))
  expect_identical(nrow(codelist1$drug), 4L)
  expect_identical(nrow(codelist1$other), 2L)

  # Expected errors
  expect_error(excludeConcepts("hola", cdm, concepts = 1L))
  expect_error(excludeConcepts(list("a" = 1L), cdm, concepts = 1L))
  expect_error(excludeConcepts(codelist, "hola", concepts = 1L))
  expect_error(excludeConcepts(codelist, cdm, concepts = "hola"))
  expect_error(excludeConcepts(codelist, cdm, concepts = 1L, codelistName = "hola"))
})
