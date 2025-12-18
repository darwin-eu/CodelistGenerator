test_that("check intersectCodelists works", {
  skip_on_cran()
  cdm <- mockVocabRef()

  # Codelist
  x <- getDrugIngredientCodes(cdm,
                              nameStyle = "{concept_name}")
  intersection <- intersectCodelists(x, keepOriginal = FALSE)
  expect_true(inherits(intersection, "codelist"))
  expect_true(intersection[[1]] == 13)
  expect_true(names(intersection) == "intersection_adalimumab_other_ingredient")

  intersection <- intersectCodelists(x, keepOriginal = TRUE)
  expect_true(all(intersection[[1]] == c(10,13)))
  expect_true(intersection[[2]] == c(13))
  expect_true(intersection[[3]] == c(13))
  expect_true(all(names(intersection) == c("adalimumab",
                                           "intersection_adalimumab_other_ingredient",
                                           "other_ingredient")))


  # Codelist with details
  x <- asCodelistWithDetails(x, cdm)
  expect_no_error(inter <- intersectCodelists(x, keepOriginal = TRUE))
  expect_identical(names(inter), c("adalimumab", "intersection_adalimumab_other_ingredient","other_ingredient"))
  expect_no_error(inter <- intersectCodelists(x, keepOriginal = FALSE))
  expect_identical(names(inter), "intersection_adalimumab_other_ingredient")
  expect_identical(inter[[1]], x[[2]])

  # no intersection codelist with details
  x[[1]] <- x[[1]] |> dplyr::mutate("ancestor" = c(1))
  x[[2]] <- x[[2]] |> dplyr::mutate("ancestor" = c(2))
  expect_no_error(inter <- intersectCodelists(x, keepOriginal = FALSE))
  expect_true(names(inter) == "intersection_adalimumab_other_ingredient")
  expect_identical(nrow(inter$intersection_adalimumab_other_ingredient), 0L)

  # No intersection
  x <- newCodelist(list("a" = c(1L,2L), "b" = c(3L,4L)))
  expect_no_error(intersectCodelists(x))

  # Expected errors
  expect_error(intersectCodelists(x, keepOriginal = "a"))
  expect_error(intersectCodelists(x, keepOriginal = c(TRUE, FALSE)))

})

test_that("intersect codelists - eunomia", {
  skip_on_cran()
  CDMConnector::requireEunomia("synpuf-1k", "5.3")
  con <- DBI::dbConnect(duckdb::duckdb(),
                        CDMConnector::eunomiaDir("synpuf-1k", "5.3"))
  cdm <- CDMConnector::cdmFromCon(con = con,
                                  cdmName = "Eunomia Synpuf",
                                  cdmSchema = "main",
                                  writeSchema = "main")

  # three codelists, one for each
  meds <- getDrugIngredientCodes(cdm,
                                 c("acetaminophen",
                                   "aspirin",
                                   "codeine"),
                                 nameStyle = "{concept_name}")
  meds_intersect <- intersectCodelists(meds)
  expect_true(length(meds_intersect) == 1)
  expect_true(names(meds_intersect) == "intersection_acetaminophen_aspirin_codeine")

  meds_2_intersect_keep <- intersectCodelists(meds, keepOriginal = TRUE)
  expect_true(length(meds_2_intersect_keep) == 4)
  expect_true(all(c("acetaminophen",
                    "aspirin",
                    "codeine",
                    "intersection_acetaminophen_aspirin_codeine") %in%
                    names(meds_2_intersect_keep)))

  omopgenerics::cdmDisconnect(cdm)
})
