subsetCodelistBy <- function(x,
                             cdm,
                             by,
                             group,
                             negate,
                             st,
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
  if (inherits(x, "code_search")) {
    x <- asCodelist(x)
  }
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
  } else if (by == "ingredient_range") {
    x <- addIngredientCount(cdm, x <- cdm[[nm]])
  }

  if (by == "ingredient_range") {
    if (isTRUE(negate)) {
      x <- x |>
        dplyr::filter(.data$ingredient_count < !!group[1] |
                      .data$ingredient_count > !!group[2])
    } else {
      x <- x |>
        dplyr::filter(.data$ingredient_count >= !!group[1],
                      .data$ingredient_count <= !!group[2])
    }
  } else {

    # correct missing
    x <- correctMissingValue(x, by)

    if(isTRUE(negate)){
      x <- x |>
        dplyr::filter(!.data[[by]] %in% omopgenerics::toSnakeCase(.env$group))
    }else{
      x <- x |>
        dplyr::filter(.data[[by]] %in% omopgenerics::toSnakeCase(.env$group))
    }
  }

  x <- stratifyCodelist(x, by = "codelist_name", nameStyle = "{codelist_name}")

  # add class (and details)
  x <- prepareCodelist(x = x, original = original, searchStrategy = st)

  if (!inherits(x, "code_search")) {
    x <- dropEmptyCodelist(original, x, call = call)
  }

  # add original codes
  if (isTRUE(keepOriginal)) {
    x <- keepOriginalCodelists(x = x, original = original)
  }

  return(x)
}

keepOriginalCodelists <- function(x, original) {
  if (inherits(x, "code_search")) {
    cols <- codeSerachColumns |>
      dplyr::filter(.data$table == "codes") |>
      dplyr::pull("table")
    codelists <- setdiff(colnames(original), cols)
    x <- x |>
      dplyr::full_join(
        original |>
          dplyr::select("concept_id", dplyr::all_of(codelists)),
        by = "concept_id",
        suffix = c(".new", ".original")
      ) |>
      dplyr::mutate(dplyr::across(
        !dplyr::all_of(cols),
        \(x) dplyr::coalesce(x, FALSE)
      ))
  } else {
    x <- c(x, original)
  }
  return(x)
}
