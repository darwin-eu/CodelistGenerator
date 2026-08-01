
# This code is experimental and will be moved to omopgenerics when stable

#' Create a new `code_search` object
#'
#' `r lifecycle::badge('experimental')`
#' This function will be moved to omopgenerics (and re-exported by
#' CodelistGenerator) once stable.
#'
#' @param codes A tibble with concept ids. It must contain "search_id",
#' "concept_id", "found_from", "vocabulary_name", "concept_name", "domain_id",
#' "vocabulary_id", "concept_class_id", "standard_concept", "concept_code",
#' "valid_start_date", "valid_end_date" and "invalid_reason" as columns.
#' @param searchStrategy A tibble with the search strategy used to derive the
#' codes. It must contain "search_id", "strategy_name" and "strategy_value" as
#' columns.
#'
#' @returns A `code_search` object.
#'
newCodeSearch <- function(codes, searchStrategy) {
  colsCodes <- codeSerachColumns |>
    dplyr::filter(.data$table == "codes") |>
    dplyr::pull("column")
  omopgenerics::assertTable(codes, columns = colsCodes)

  colsStrategy <- codeSerachColumns |>
    dplyr::filter(.data$table == "search_strategy") |>
    dplyr::pull("column")
  omopgenerics::assertTable(searchStrategy, columns = colsStrategy)

  # make them tibbles
  codes <- dplyr::as_tibble(codes) |>
    dplyr::relocate(dplyr::all_of(colsCodes))
  searchStrategy <- dplyr::as_tibble(searchStrategy) |>
    dplyr::relocate(dplyr::all_of(colsStrategy))

  # cast columns
  codes <- castColumns(codes, "codes")
  searchStrategy <- castColumns(searchStrategy, "search_strategy")

  structure(
    .Data = codes,
    search_strategy = searchStrategy,
    class = c("code_search", class(codes))
  )
}

validateCodeSearch <- function(codeSearch) {
  omopgenerics::assertClass(codeSearch, "code_search")
}

#' Empty code search object
#'
#' @param searchStrategy The search strategy used to generate the candidate
#' codes.
#'
#' @returns An empty `code_search` object.
#' @export
#'
emptyCodeSearch <- function(searchStrategy = NULL) {
  if (is.null(searchStrategy)) {
    searchStrategy <- dplyr::tibble(
      strategy_id = integer(),
      strategy_name = character(),
      strategy_value = character()
    )
  }
  dplyr::tibble(
    concept_id = integer(),
    found_from = character(),
    concept_name = character(),
    vocabulary_version = character(),
    domain_id = character(),
    vocabulary_id = character(),
    concept_class_id = character(),
    standard_concept = character(),
    concept_code = character(),
    valid_start_date = as.Date(character()),
    valid_end_date = as.Date(character()),
    invalid_reason = character()
  ) |>
    newCodeSearch(searchStrategy = searchStrategy)
}

castColumns <- function(x, table) {
  cols <- codeSerachColumns |>
    dplyr::filter(.data$table == .env$table)
  q <- paste0(cols$convert, "(.data$", cols$column, ")") |>
    rlang::parse_exprs() |>
    rlang::set_names(nm = cols$column)
  x |>
    dplyr::mutate(!!!q)
}

#' @export
print.code_search <- function(x, ...) {
  # info
  cat("\033[1;34mi\033[0m This is a candidate code searh, see: <link to vignette>\n")
  # search strategy
  st <- searchStrategy(x) |>
    dplyr::filter(.data$strategy_id == 1L) |>
    tidyr::pivot_wider(names_from = "strategy_name", values_from = "strategy_value")
  if (nrow(st) > 0) {
    pkg <- paste0("\033[32m", st$package_name %||% "unknown package", "\033[0m (\033[1m",  st$package_version %||% "unknown version", "\033[0m)")
    cat(paste0("\033[3mCandidate codes\033[0m generated using ", pkg, ":\n"))
    fun <- st$function_name %||% ""
    parameters <- st |>
      dplyr::select(!dplyr::any_of(c("package_name", "package_version", "strategy_id", "function_name"))) |>
      purrr::imap_chr(\(x, nm) paste0(nm, " = ", x)) |>
      paste0(collapse = ",\n  ")
    cat(paste0(fun, "(\n  ", parameters, "\n)\n"))
  } else {
    cat("\033[3mNo search strategy found.\033[0m\n")
  }
  NextMethod()
}

#' Import a `code_search` object from an Excel spreadsheet
#'
#' `r lifecycle::badge('experimental')`
#' This function will be moved to omopgenerics (and re-exported by
#' CodelistGenerator) once stable.
#'
#' @param path Path to the Excel spreadsheet.
#'
#' @returns A `code_search` object.
#' @export
#'
importCodeSearch <- function(path) {
  omopgenerics::assertCharacter(path, length = 1)
  rlang::check_installed("openxlsx")

  if (file.exists(path)) {
    file <- path
  } else if (file.exists(paste0(path, ".xlsx"))) {
    file <- paste0(path, ".xlsx")
  } else if (file.exists(paste0(path, ".xls"))) {
    file <- paste0(path, ".xls")
  } else {
    if (dir.exists(path)) {
      file <- list.files(path, pattern = ".xls$|.xlsx$", full.names = TRUE)
      if (length(file) > 1) {
        cli::cli_abort(c(x = "Multiple Excel spreadsheets found in {.path {path}} please specify just one."))
      }
    } else {
      cli::cli_abort(c(x = "Provided file/path does not exist."))
    }
  }

  sheets <- openxlsx::getSheetNames(file = file)
  notPresent <- c("SearchStrategy", "CandidateCodes") |>
    purrr::keep(\(x) !x %in% sheets)
  if (length(notPresent) > 0) {
    cli::cli_abort(c(x = "{.var {notPresent}} sheet{?s} not present in excel file."))
  }

  newCodeSearch(
    codes = openxlsx::read.xlsx(xlsxFile = file, sheet = "CandidateCodes", detectDates = TRUE),
    searchStrategy = openxlsx::read.xlsx(xlsxFile = file, sheet = "SearchStrategy")
  )
}

#' Export a `code_search` object into an Excel spreadsheet
#'
#' `r lifecycle::badge('experimental')`
#' This function will be moved to omopgenerics (and re-exported by
#' CodelistGenerator) once stable.
#'
#' @param codeSearch A `code_search` object.
#' @param file Name of the Excel spreadsheet to be created.
#' @param path Path where to create the Excel file.
#'
#' @returns The `code_search` object is exported to a file.
#' @export
#'
exportCodeSearch <- function(codeSearch, file, path = getwd()) {
  # initial checks
  codeSearch <- validateCodeSearch(codeSearch)
  omopgenerics::assertCharacter(file, length = 1)

  rlang::check_installed("openxlsx")

  if (!endsWith(file, ".xlsx") & !endsWith(file, ".xls")) {
    file <- paste0(file, ".xlsx")
  }

  # add instructions
  wb <- openxlsx::loadWorkbook(
    file = system.file("Instructions.xlsx", package = "CodelistGenerator")
  )

  # add search strategy
  openxlsx::addWorksheet(wb = wb, sheetName = "SearchStrategy")
  openxlsx::writeData(wb = wb, sheet = "SearchStrategy", x = searchStrategy(codeSearch))

  # add candidate codes
  openxlsx::addWorksheet(wb = wb, sheetName = "CandidateCodes")
  openxlsx::writeData(wb = wb, sheet = "CandidateCodes", x = codeSearch)

  openxlsx::saveWorkbook(wb = wb, file = file.path(path, file), overwrite = TRUE)
}


#' Get the search strategy used to create a certain code_search
#'
#' `r lifecycle::badge('experimental')`
#' This function will be moved to omopgenerics (and re-exported by
#' CodelistGenerator) once stable.
#'
#' @param codeSearch A `code_search` object.
#'
#' @returns A search strategy tibble.
#' @export
#'
searchStrategy <- function(codeSearch) {
  validateCodeSearch(codeSearch = codeSearch)
  attr(codeSearch, "search_strategy")
}

cast <- function(x) {
  if (length(x) == 0) {
    x <- cast0(x)
  } else if (length(x) == 1) {
    x <- cast1(x)
  } else {
    x <- paste0("c(", paste0(purrr::map_chr(x, cast1), collapse = ", "), ")")
  }
  return(x)
}
cast0 <- function(x) {
  if (is.null(x)) {
    x <- "NULL"
  } else if (is.logical(x)) {
    x <- "logical()"
  } else if (is.integer(x)) {
    x <- "integer()"
  } else if (is.numeric(x)) {
    x <- "numeric()"
  } else if (is.character(x)) {
    x <- "character()"
  } else {
    cli::cli_abort(c(x = "Not indeitified type: {.cls {class(x)}}"))
  }
  return(x)
}
cast1 <- function(x) {
  if (is.logical(x)) {
    x <- as.character(x)
  } else if (is.integer(x)) {
    x <- sprintf("%iL", x)
  } else if (is.numeric(x)) {
    x <- as.character(x)
  } else if (is.character(x)) {
    x <- paste0('"', x, '"')
  } else {
    cli::cli_abort(c(x = "not indeitified type: {.cls {class(x)}}"))
  }
  return(x)
}

prepareAsCodelist <- function(x) {
  comp <- codeSerachColumns |>
    dplyr::filter(.data$table == "codes") |>
    dplyr::pull("column")
  cols <- colnames(x)
  cols <- cols[!cols %in% comp]

  if (length(cols) == 0) {
    cli::cli_inform(c("!" = "No column specifying T/F for the different codes is detected"))
    return(list())
  }

  cols <- rlang::set_names(cols, omopgenerics::toSnakeCase(x = cols))

  cols |>
    purrr::map(\(col) {
      x |>
        dplyr::filter(as.logical(.data[[col]])) |>
        dplyr::select(dplyr::any_of(comp))
    })
}

#' @export
asCodelist.code_search <- function(x, ...) {
  x |>
    prepareAsCodelist() |>
    purrr::map(\(x) as.integer(x$concept_id)) |>
    newCodelist()
}

#' @export
asConceptSetExpression.code_search <- function(x, ...) {
  x |>
    prepareAsCodelist() |>
    # purrr::map(\(x) {
    #   x |>
    #     dplyr::mutate(descendants = FALSE, exclude = FALSE, mapped = FALSE)
    # }) |>
    newConceptSetExpression()
}

#' @export
asCodelistWithDetails.code_search <- function(x, ...) {
  x |>
    prepareAsCodelist() |>
    newCodelistWithDetails()
}
