subsetCodelistBy <- function(x,
                             cdm,
                             by,
                             group,
                             negate,
                             keepOriginal = FALSE,
                             call = parent.frame()){
  # initial checks
  checkCodelist(x, allowConceptSetExpression = FALSE, call = call)
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm, call = call)
  omopgenerics::assertLogical(negate, length = 1, call = call)
  omopgenerics::assertLogical(keepOriginal, length = 1, call = call)

  if (length(x) == 0) {
    cli::cli_warn(c("!" = "Empty codelist provided. Returning an empty codelist."), call = call)
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
  } else if (by == "dose_form") {
    x <- addDoseForm(x = cdm[[nm]])
  } else if (by == "dose_unit") {
    x <- addDoseUnit(x = cdm[[nm]])
  } else if (by == "vocabulary") {
    x <- addVocabulary(x = cdm[[nm]])
  } else if (by == "route_category") {
    x <- addRouteCategory(x = cdm[[nm]])
  }

  # correct missing
  x <- correctMissingValue(x, by)

  if(isTRUE(negate)){
    x <- x |>
      dplyr::filter(!.data[[by]] %in% omopgenerics::toSnakeCase(.env$group))
  }else{
    x <- x |>
      dplyr::filter(.data[[by]] %in% omopgenerics::toSnakeCase(.env$group))
  }

  x <- stratifyCodelist(x, by = "codelist_name", nameStyle = "{codelist_name}")

  # add class (and details)
  x <- prepareCodelist(x = x, original = original)

  x <- dropEmptyCodelist(original, x, call = call)

  # add original codes
  if (isTRUE(keepOriginal)) {
    x <- c(x, original)
  }
  return(x)
}
