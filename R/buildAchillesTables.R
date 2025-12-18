
buildAchillesTables <- function(cdm,
                                achillesId = NULL) {
  # initial check
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  achillesId <- validateAchillesId(achillesId = achillesId)

  # check existent tables
  cdm <- checkExistentAchillesTables(cdm = cdm)

  # remove repeated results
  cdm <- removeRepeatedIds(cdm = cdm, achillesId = achillesId)

  # create new analysis
  len <- sprintf("%i", length(achillesId))
  for (k in seq_along(achillesId)) {
    id <- achillesId[k]
    nm <- achillesAnalisisDetails |>
      dplyr::filter(.data$analysis_id == .env$id) |>
      dplyr::pull("analysis_name")
    kk <- sprintf("%i", k) |>
      stringr::str_pad(width = nchar(len), pad = " ")
    tab <- achillesAnalisisDetails$table[k]
    if(tab %in% names(cdm)){
      cli::cli_inform(c("i" = "{kk} of {len}: Get achilles result for {.pkg {nm}}."))
      cdm <- appendAchillesId(cdm, id)
    }else{
      cli::cli_inform(c("i" = "{kk} of {len}: Skipping achilles results for {.pkg {nm}} because the cdm doesn't contain table {tab}."))
    }
  }

  # append to achilles_analysis
  name <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = name,
    table = achillesAnalisisDetails |>
      dplyr::filter(.data$analysis_id %in% .env$achillesId) |>
      dplyr::select(!c("table", "type", "distribution", "distributed_field"))
  )
  cdm[["achilles_analysis"]] <- cdm[["achilles_analysis"]] |>
    dplyr::union_all(cdm[[name]]) |>
    dplyr::compute(name = "achilles_analysis")
  cdm <- omopgenerics::dropSourceTable(cdm = cdm, name = name)

  return(cdm)
}

validateAchillesId <- function(achillesId, call = parent.frame()) {
  # possible ids
  possibleIds <- achillesAnalisisDetails$analysis_id

  # default analysis_id
  if (is.null(achillesId)) {
    achillesId <- possibleIds
  } else {
    achillesId <- as.integer(achillesId)
    omopgenerics::assertNumeric(achillesId, integerish = TRUE, unique = TRUE, call = call)
    ignored <- achillesId[!achillesId %in% possibleIds]
    if (length(ignored) > 0) {
      cli::cli_inform(c("i" = "{length(ignored)} analysis id{?s} {?is/are} not configured: {ignored}."))
    }
    achillesId <- achillesId[achillesId %in% possibleIds]
  }

  achillesId
}
checkExistentAchillesTables <- function(cdm) {
  notPresent <- omopgenerics::achillesTables() |>
    purrr::keep(\(x) !x %in% names(cdm))
  if (length(notPresent) > 0) {
    possibleToRead <- notPresent[notPresent %in% omopgenerics::listSourceTables(cdm = cdm)]
    if (length(possibleToRead) > 0) {
      cli::cli_inform(c(i = "Reading tables from source: {.pkg {possibleToRead}}"))
      cdm <- omopgenerics::readSourceTable(cdm = cdm, name = possibleToRead)
    }
    needToCreate <- purrr::keep(notPresent, \(x) !x %in% names(cdm))
    if (length(needToCreate) > 0) {
      for (nm in needToCreate) {
        cli::cli_inform(c(i = "Creating empty {.pkg {nm}} table."))
        cdm <- omopgenerics::emptyAchillesTable(cdm = cdm, name = nm)
      }
    }
  }
  return(cdm)
}
removeRepeatedIds <- function(cdm, achillesId) {
  repeatedIds <- list(
    achilles_analysis = cdm[["achilles_analysis"]] |>
      dplyr::distinct(.data$analysis_id) |>
      dplyr::pull(),
    achilles_results = cdm[["achilles_results"]] |>
      dplyr::distinct(.data$analysis_id) |>
      dplyr::pull(),
    achilles_results_dist = cdm[["achilles_results_dist"]] |>
      dplyr::distinct(.data$analysis_id) |>
      dplyr::pull()
  ) |>
    purrr::map(\(x) x[x %in% achillesId]) |>
    purrr::compact()
  if (length(repeatedIds) > 0) {
    for (nm in names(repeatedIds)) {
      ids <- repeatedIds[[nm]]
      "Removing {length(ids)} repeated analysis ids from {.pkg {nm}}." |>
        rlang::set_names("!") |>
        cli::cli_inform()
      cdm[[nm]] <- cdm[[nm]] |>
        dplyr::filter(!.data$analysis_id %in% .env$ids) |>
        dplyr::compute(name = nm)
    }
  }
  return(cdm)
}
appendAchillesId <- function(cdm, id) {
  # get analysis results
  analysis <- achillesAnalisisDetails |>
    dplyr::filter(.data$analysis_id == .env$id)

  tableName <- analysis$table
  by <- groupBy(analysis)
  mut <- mutateColumns(analysis)

  fun <- switch(analysis$type,
                "record_count" = "dplyr::n()",
                "person_count" = "dplyr::n_distinct(.data$person_id)")
  q <- paste0("as.integer(", fun, ")") |>
    rlang::set_names("count_value") |>
    rlang::parse_exprs()

  nm <- omopgenerics::uniqueTableName()
  res <- cdm[[tableName]] |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(!!!q) |>
    dplyr::mutate(!!!mut, analysis_id = !!analysis$analysis_id) |>
    dplyr::compute(name = nm) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("stratum"), as.character))

  cdm[["achilles_results"]] <- cdm[["achilles_results"]] |>
    dplyr::union_all(res) |>
    dplyr::compute(name = "achilles_results")

  omopgenerics::dropSourceTable(cdm = cdm, name = nm)

  return(cdm)

}
groupBy <- function(analysis) {
  by <- analysis |>
    dplyr::select(dplyr::starts_with("stratum_")) |>
    as.list() |>
    unlist() |>
    purrr::keep(\(x) !is.na(x))
  names(by) <- stringr::str_remove(names(by), "_name$")
  return(by)
}
mutateColumns <- function(analysis) {
  mut <- analysis |>
    dplyr::select(dplyr::starts_with("stratum_")) |>
    as.list() |>
    purrr::keep(\(x) is.na(x))
  names(mut) <- stringr::str_remove(names(mut), "_name$")
  return(mut)
}
