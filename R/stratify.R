stratifyCodelistBy <- function(x,
                               cdm,
                               by,
                               nameStyle,
                               keepOriginal,
                               call = parent.frame()) {
  # initial checks
  checkCodelist(x, allowConceptSetExpression = FALSE, call = call)
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm, call = call)
  omopgenerics::assertLogical(keepOriginal, length = 1, call = call)
  nameStyle <- prepareNameStyle(nameStyle = nameStyle, call = call)

  if (length(x) == 0) {
    cli::cli_warn(c("!" = "Empty codelist provided. Returning an empty codelist."))
    return(x)
  }

  original <- x

  # codelist table
  nm <- omopgenerics::uniqueTableName()
  x <- dplyr::as_tibble(x) |>
    dplyr::rename(codelist_name = dplyr::any_of("codelist_with_details_name")) |>
    dplyr::select("codelist_name", "concept_id")
  cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = x)
  on.exit(omopgenerics::dropSourceTable(cdm = cdm, name = nm))

  if (by == "domain") {
    x <- addDomain(x = cdm[[nm]])
  } else if (by == "brand") {
    x <- addBrand(x = cdm[[nm]])
  } else if (by == "dose_form") {
    x <- addDoseForm(x = cdm[[nm]])
  } else if (by == "dose_unit") {
    x <- addDoseUnit(x = cdm[[nm]])
  } else if (by == "vocabulary") {
    x <- addVocabulary(x = cdm[[nm]])
  } else if (by == "route_category") {
    x <- addRouteCategory(x = cdm[[nm]])
  } else if (by == "concept") {
    x <- addConcept(x = cdm[[nm]])
  }

  # correct missing
  x <- correctMissingValue(x, by)

  # check nameStyle
  checkNameStyleStratify(nameStyle = nameStyle, x = x, by = by, call = call)

  # warn concepts in multiple domains
  warnMultiple(x = x, by = by)

  # stratify codelist
  x <- stratifyCodelist(
    x = x,
    by = c("codelist_name", by),
    nameStyle = nameStyle
  )

  # add class (and details)
  x <- prepareCodelist(x = x, original = original)

  # add original codes
  if (isTRUE(keepOriginal)) {
    x <- c(x, original)
  }

  return(x)
}

correctMissingValue <- function(x, by){
  missingValue <- paste("Unclassified", by)
  x <- x |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(by),
      .fns = \(x) omopgenerics::toSnakeCase(dplyr::coalesce(x, missingValue))
    ))

  return(x)
}
prepareNameStyle <- function(nameStyle, call) {
  omopgenerics::assertCharacter(nameStyle, length = 1, call = call)
  nameStyle |>
    stringr::str_replace_all(
      pattern = stringr::fixed("{codelist_with_details_name}"),
      replacement = "{codelist_name}"
    )
}
checkNameStyleStratify <- function(nameStyle, x, by, call) {
  args <- list(
    nameStyle = nameStyle,
    codelist_name = unique(x$codelist_name),
    call = call
  )
  args[[by]] <- unique(x[[by]])
  do.call(omopgenerics::validateNameStyle, args)
}
warnMultiple <- function(x, by) {
  concepts <- x |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c("concept_id", by)))) |>
    dplyr::group_by(.data$concept_id) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::distinct(.data$concept_id) |>
    dplyr::pull()
  if (length(concepts) > 0) {
    cli::cli_warn("{length(concepts)} concept ID{?s} have duplicated `{by}` and will be assigned to multiple codelists ({concepts}).")
  }
  invisible()
}
stratifyCodelist <- function(x, by, nameStyle) {
  # split codelists
  x <- x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::group_split() |>
    as.list()

  # create names
  nms <- x |>
    purrr::map_chr(\(x) {
      x |>
        dplyr::distinct(dplyr::across(dplyr::all_of(by))) |>
        dplyr::mutate(name = glue::glue(nameStyle)) |>
        dplyr::pull("name")
    })
  names(x) <- nms

  return(x)
}
prepareCodelist <- function(x, original) {
  if (inherits(original, "codelist")) {
    x <- x |>
      purrr::map(\(x) x$concept_id) |>
      omopgenerics::newCodelist()
  } else {
    x <- x |>
      purrr::map(\(x) {
        nm <- unique(x$codelist_name)
        x |>
          dplyr::select("concept_id") |>
          dplyr::mutate(concept_id = as.integer(.data$concept_id)) |>
          dplyr::inner_join(
            original[[nm]] |>
              dplyr::mutate(concept_id = as.integer(.data$concept_id)),
            by = "concept_id"
          )
      }) |>
      omopgenerics::newCodelistWithDetails()
  }
  return(x)
}
