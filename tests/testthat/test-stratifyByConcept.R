test_that("multiplication works", {

 cdm <- mockVocabRef()
 cl <- omopgenerics::newCodelist(list(a = c(1,2,3),
       b = c(3,4,5)))

 cl_s1 <- stratifyByConcept(cl, cdm, keepOriginal = FALSE)
 expect_true(length(cl_s1) == 6)


 cl_s2 <- stratifyByConcept(cl, cdm, keepOriginal = TRUE)
 expect_true(length(cl_s2) == 8)
 expect_true(all(sort(names(cl)) == sort(setdiff(names(cl_s2), names(cl_s1)))))



 cl <- omopgenerics::newCodelistWithDetails(list(a = data.frame(concept_id = c(1,2,3),
                                                                concept_name = c("a", "b", "c")),
                                 b =  data.frame(concept_id = c(1,2,3),
                                                 concept_name = c("c", "d", "e"))))

 cl_s1 <- stratifyByConcept(cl, cdm, keepOriginal = FALSE)
 expect_true(length(cl_s1) == 6)


 cl_s2 <- stratifyByConcept(cl, cdm, keepOriginal = TRUE)
 expect_true(length(cl_s2) == 8)
 expect_true(all(sort(names(cl)) == sort(setdiff(names(cl_s2), names(cl_s1)))))

 # if concepts are not in the cdm
 cdm <- mockVocabRef()
 cl <- omopgenerics::newCodelist(list(a = c(1,2,3),
                                      b = c(3,4,5,99999)))
 expect_warning(cl_s1 <- stratifyByConcept(cl, cdm, keepOriginal = FALSE))
 expect_true(length(cl_s1) == 6) # concept 99999 will have been dropped

})
