test_that("union codelists - mock", {
  skip_on_cran()
  cdm <- mockVocabRef(backend = "data_frame")

  ing_codes <- getDrugIngredientCodes(cdm,
                                      nameStyle = "{concept_name}")

  # codelist
  ing_codes_union <- unionCodelists(ing_codes)
  expect_true(inherits(ing_codes_union, "codelist"))
  expect_true(length(ing_codes_union) == 1)
  expect_identical(ing_codes_union[[1]],
                   unique(c(ing_codes[[1]], ing_codes[[2]])))

  # codelist with details
  x1 <- asCodelistWithDetails(ing_codes, cdm)
  x1 <- unionCodelists(x1)
  expect_true(inherits(x1, "codelist_with_details"))
  expect_true(length(x1) == 1)
  expect_identical(x1[[1]],
                   asCodelistWithDetails(ing_codes, cdm)[[1]])

  # note, when codelists will come out in alphabetical order
  # so the unioned codelist will not necessarily be at the end
  ing_codes_union_2 <- unionCodelists(ing_codes,
                                      keepOriginal = TRUE)
  expect_true(length(ing_codes_union_2) == 3)
  expect_identical(ing_codes_union_2[["adalimumab_other_ingredient"]],
                   unique(c(ing_codes_union_2[["adalimumab"]],
                            ing_codes_union_2[["other_ingredient"]])))
  expect_identical(ing_codes_union_2[["adalimumab"]],
                   ing_codes[["adalimumab"]])
  expect_identical(ing_codes_union_2[["other_ingredient"]],
                   ing_codes[["other_ingredient"]])


  # expected errors
  expect_error(unionCodelists(ing_codes, keepOriginal = "a"))
  expect_error(unionCodelists(ing_codes, keepOriginal = c(TRUE, FALSE)))

  })

test_that("union codelists - eunomia", {
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
  meds_union <- unionCodelists(meds)
  expect_true(length(meds_union) == 1)
  expect_true(names(meds_union) == "acetaminophen_aspirin_codeine")

  meds_2_union_keep <- unionCodelists(meds, keepOriginal = TRUE)
  expect_true(length(meds_2_union_keep) == 4)
  expect_true(all(c("acetaminophen",
                "aspirin",
                "codeine",
                "acetaminophen_aspirin_codeine") %in%
                names(meds_2_union_keep)))


  omopgenerics::cdmDisconnect(cdm)
})
