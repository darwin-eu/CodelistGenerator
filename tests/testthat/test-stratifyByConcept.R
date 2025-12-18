test_that("multiplication works", {
  skip_on_cran()
  cdm <- mockVocabRef()
  cl <- omopgenerics::newCodelist(list(a = c(1L,2L,3L),
                                       b = c(3L,4L,5L)))

  cl_s1 <- stratifyByConcept(cl, cdm, keepOriginal = FALSE)
  expect_true(length(cl_s1) == 6)


  cl_s2 <- stratifyByConcept(cl, cdm, keepOriginal = TRUE)
  expect_true(length(cl_s2) == 8)
  expect_true(all(sort(names(cl)) == sort(setdiff(names(cl_s2), names(cl_s1)))))



  cl <- omopgenerics::newCodelistWithDetails(list(a = data.frame(concept_id = c(1L,2L,3L),
                                                                 concept_name = c("a", "b", "c")),
                                                  b =  data.frame(concept_id = c(1L,2L,3L),
                                                                  concept_name = c("c", "d", "e"))))

  cl_s1 <- stratifyByConcept(cl, cdm, keepOriginal = FALSE)
  expect_true(length(cl_s1) == 6)


  cl_s2 <- stratifyByConcept(cl, cdm, keepOriginal = TRUE)
  expect_true(length(cl_s2) == 8)
  expect_true(all(sort(names(cl)) == sort(setdiff(names(cl_s2), names(cl_s1)))))

  # if concepts are not in the cdm
  cdm <- mockVocabRef()
  cl <- omopgenerics::newCodelist(list(a = c(1L, 2L, 3L), b = c(3L, 4L, 5L, 99999L)))
  #expect_warning(cl_s1 <- stratifyByConcept(cl, cdm, keepOriginal = FALSE))
  #expect_true(inherits(cl_s1, "codelist"))
  #expect_true(length(cl_s1) == 6) # concept 99999 will have been dropped

  # if concepts are a codelist with details
  x <- omopgenerics::newCodelist(list("a" = c(1L,2L,3L)))
  x <- asCodelistWithDetails(x, cdm)
  expect_no_error(cl_s2 <- stratifyByConcept(x, cdm, keepOriginal = FALSE))
  expect_true(inherits(cl_s2, "codelist_with_details"))
  expect_identical(cl_s2$a_arthritis,
                   dplyr::tibble(
                     "concept_id" = 3L,
                     "concept_name" = "Arthritis",
                     "domain_id" = "Condition",
                     "vocabulary_id" = "SNOMED",
                     "standard_concept" = "standard"
                   ))

  # if concepts
  # expected errors
  expect_error(stratifyByConcept(list("a" = c(1L,2L,3L)), cdm))
  expect_error(stratifyByConcept(omopgenerics::newCodelistlist("a" = c(1L,2L,3L)),
                                 cdm = "a",
                                 keepOriginal = FALSE))
  expect_error(stratifyByConcept(omopgenerics::newCodelistlist("b" = c(1L,2L,3L)),
                                 cdm = cdm,
                                 keepOriginal = "a"))

})
