
test_that("emptyCodeSearch returns a code_search object", {
  skip_on_cran()

  # empty object
  expect_no_error(cs <- emptyCodeSearch())
  expect_s3_class(cs, "code_search")
  expect_equal(nrow(cs), 0L)
  cols <- codeSerachColumns$column[codeSerachColumns$table == "codes"]
  expect_true(all(cols %in% colnames(cs)))
  st <- searchStrategy(cs)
  expect_equal(nrow(st), 0L)
  cols <- codeSerachColumns$column[codeSerachColumns$table == "search_strategy"]
  expect_true(all(cols %in% colnames(st)))
  cs1 <- emptyCodeSearch(searchStrategy = NULL)
  expect_identical(cs, cs1)

  # empty object with code search
  st <- dplyr::tibble(
    strategy_id = 1L,
    strategy_name = c("package_name", "function_name", "package_version"),
    strategy_value = c("CodelistGenerator", "search", "1.0")
  )
  expect_no_error(cs <- emptyCodeSearch(searchStrategy = st))
  expect_equal(nrow(searchStrategy(cs)), 3)
  expect_identical(searchStrategy(cs), st)

  # helper
  codesTibble <- function(n = 10) {
    dplyr::tibble(
      concept_id = as.integer(seq_len(n)),
      found_from = "Descendants",
      concept_name = paste0("Concept ", seq_len(n)),
      vocabulary_version = "v1.0",
      domain_id = "Condition",
      vocabulary_id = "SNOMED",
      concept_class_id = "Clinical Finding",
      standard_concept = "S",
      concept_code = paste0("C00", seq_len(n)),
      valid_start_date = as.Date("1970-01-01"),
      valid_end_date = as.Date("2099-12-31"),
      invalid_reason = NA_character_
    )
  }

  expect_no_error(cs <- newCodeSearch(codes = codesTibble(), searchStrategy = st))
  expect_true(nrow(cs) == 10)
  expect_true(nrow(searchStrategy(cs)) == 3)
  expect_s3_class(cs, "code_search")
  expect_s3_class(cs, "tbl_df")
  expect_s3_class(searchStrategy(cs), "tbl_df")
  expect_identical(searchStrategy(cs), st)

  # print works
  expect_output(print(cs))
  expect_output(print(emptyCodeSearch()))

  # errors
  bad_codes <- dplyr::tibble(concept_id = 1L)  # missing many required columns
  expect_error(newCodeSearch(codes = bad_codes, searchStrategy = st))
  expect_error(searchStrategy(list()))
  expect_error(searchStrategy(dplyr::tibble()))
  expect_error(cs <- newCodeSearch(codes = codesTibble(), searchStrategy = NULL))
  bad_strategy <- dplyr::tibble(strategy_name = "keyword")  # missing strategy_id, strategy_value
  expect_error(newCodeSearch(codes = makeCodesTibble(), searchStrategy = bad_strategy))

  # cast
  codes <- codesTibble()
  codes$concept_id <- as.double(codes$concept_id)
  codes$valid_start_date <- as.double(codes$valid_start_date)
  cs <- newCodeSearch(codes = codes, searchStrategy = st)
  expect_type(cs$concept_id, "integer")
  expect_s3_class(cs$valid_start_date, "Date")

  # import and export
  tmp <- tempdir()
  exportCodeSearch(cs, file = "test_export", path = tmp)
  expect_true(file.exists(file.path(tmp, "test_export.xlsx")))
  cs0 <- importCodeSearch(path = file.path(tmp, "test_export.xlsx"))
  expect_identical(cs, cs0)

  exportCodeSearch(cs, file = "test_export2.xlsx", path = tmp)
  expect_true(file.exists(file.path(tmp, "test_export2.xlsx")))
  cs1 <- importCodeSearch(path = file.path(tmp, "test_export2.xlsx"))
  expect_identical(cs, cs1)

  expect_error(importCodeSearch(path = "/nonexistent/path/file.xlsx"))

  bad_file <- file.path(tmp, "bad.xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "OnlyThisSheet")
  openxlsx::saveWorkbook(wb, bad_file, overwrite = TRUE)
  expect_error(importCodeSearch(path = bad_file))

  # extra sheets not a problem
  file <- file.path(tmp, "test_export2.xlsx")
  wb <- openxlsx::loadWorkbook(file = file)
  openxlsx::addWorksheet(wb = wb, sheetName = "ExtraSheet")
  openxlsx::writeData(wb = wb, sheet = "ExtraSheet", x = cars) |>
  openxlsx::saveWorkbook(wb = wb, file = file, overwrite = TRUE)
  expect_true(file.exists(file))
  expect_true("ExtraSheet" %in% openxlsx::getSheetNames(file))
  expect_no_error(cs2 <- importCodeSearch(path = file))
  expect_identical(cs, cs2)

  # asCodelist
  codes <- codesTibble(n = 4)
  codes$new_codelist <- c(TRUE, FALSE, TRUE, FALSE)
  cs <- newCodeSearch(codes = codes, searchStrategy = st)
  expect_no_error(cd <- asCodelist(cs))
  expect_s3_class(cd, "codelist")
  expect_true("new_codelist" %in% names(cd))
  expect_identical(cd[["new_codelist"]], c(1L, 3L))
  cs <- cs |>
    dplyr::mutate(second_codelist = concept_id %% 2 == 0)
  expect_no_error(cd <- asCodelist(cs))
  expect_true("new_codelist" %in% names(cd))
  expect_true("second_codelist" %in% names(cd))
  expect_true(length(cd) == 2)
  expect_identical(cd[["second_codelist"]], c(2L, 4L))

  # asConceptSetExpression
  codes <- codesTibble(n = 4)
  codes$new_codelist <- c(TRUE, FALSE, TRUE, FALSE)
  cs <- newCodeSearch(codes = codes, searchStrategy = st)
  expect_no_error(cd <- asConceptSetExpression(cs))
  expect_s3_class(cd, "concept_set_expression")
  expect_true("new_codelist" %in% names(cd))
  expect_true(all(c("concept_id", "excluded", "descendants", "mapped") %in% colnames(cd$new_codelist)))
  cs <- cs |>
    dplyr::mutate(second_codelist = concept_id %% 2 == 0)
  expect_no_error(cd <- asConceptSetExpression(cs))
  expect_true("new_codelist" %in% names(cd))
  expect_true("second_codelist" %in% names(cd))
  expect_true(length(cd) == 2)

  # asCodelistWithDetails
  codes <- codesTibble(n = 4)
  codes$new_codelist <- c(TRUE, FALSE, TRUE, FALSE)
  cs <- newCodeSearch(codes = codes, searchStrategy = st)
  expect_no_error(cd <- asCodelistWithDetails(cs))
  expect_s3_class(cd, "codelist_with_details")
  expect_true("new_codelist" %in% names(cd))
  cols <- codeSerachColumns$column[codeSerachColumns$table == "codes"]
  expect_true(all(cols %in% colnames(cd$new_codelist)))
  cs <- cs |>
    dplyr::mutate(second_codelist = concept_id %% 2 == 0)
  expect_no_error(cd <- asCodelistWithDetails(cs))
  expect_true("new_codelist" %in% names(cd))
  expect_true("second_codelist" %in% names(cd))
  expect_true(length(cd) == 2)

  # cast helper
  expect_equal(cast(NULL), "NULL")
  expect_equal(cast(character()), "character()")
  expect_equal(cast(integer()), "integer()")
  expect_equal(cast(1L), "1L")
  expect_equal(cast("hello"), '"hello"')
  expect_equal(cast(c(1L, 2L, 3L)), "c(1L, 2L, 3L)")
})
