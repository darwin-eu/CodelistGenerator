# Copyright 2025 DARWIN EU®
#
# This file is part of CodelistGenerator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' Get concept ids from JSON files containing concept sets
#' `r lifecycle::badge("deprecated")`
#'
#' @param path Path to a file or folder containing JSONs of concept sets.
#' @inheritParams cdmDoc
#' @inheritParams typeBroadDoc
#'
#' @return Named list with concept_ids for each concept set.
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omock)
#'
#' # Create a CDM object
#' cdm <- mockCdmReference()
#'
#' # Load JSON files
#' x <- codesFromConceptSet(cdm = cdm,
#'                          path =  system.file(package = "CodelistGenerator",
#'                                              "concepts_for_mock"))
#' x
#'
#' # Load JSON files as codelist_with_details
#' x <- codesFromConceptSet(cdm = cdm,
#'                          path =  system.file(package = "CodelistGenerator",
#'                                              "concepts_for_mock"),
#'                          type = "codelist_with_details")
#' x
#'
#' # Load JSON files as concept_set_expression
#' x <- codesFromConceptSet(cdm = cdm,
#'                          path =  system.file(package = "CodelistGenerator",
#'                                              "concepts_for_mock"),
#'                          type = "concept_set_expression")
#' x
#'
#' }
codesFromConceptSet <- function(path,
                                cdm,
                                type = c("codelist")) {

  lifecycle::deprecate_warn(what = "codesFromConceptSet()",
                            when = "4.0.0",
                            with = I("omopgenerics::importConceptSetExpression() |> asCodelist()"))


  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertCharacter(type, length = 1)
  omopgenerics::assertChoice(type, choices = c("codelist", "codelist_with_details", "concept_set_expression"))
  checkInputs(path = path, cdm = cdm)

  if (dir.exists(path)) {
    conceptSets <- dplyr::tibble(concept_set_path = list.files(
      path = path,
      full.names = TRUE
    ))
  } else {
    conceptSets <- dplyr::tibble(concept_set_path = .env$path)
  }
  conceptSets <- conceptSets |>
    dplyr::filter(tools::file_ext(.data$concept_set_path) == "json") |>
    dplyr::mutate(
      concept_set_name =
        tools::file_path_sans_ext(basename(.data$concept_set_path))
    ) |>
    dplyr::mutate(cohort_definition_id = dplyr::row_number())
  if (conceptSets |> nrow() == 0) {
    cli::cli_abort(glue::glue("No 'json' file found in {path}"))
  }

  # first part: read jsons
  tryCatch(
    expr = conceptList <- readConceptSet(conceptSets),
    error = function(e) {
      cli::cli_abort(
        "The json file is not a properly formated OMOP concept set."
      )
    }
  )

  if(type == "concept_set_expression"){
    conceptList <- conceptList |>
      dplyr::select("concept_id",
                    "excluded" = "is_excluded",
                    "descendants" = "include_descendants",
                    "mapped" = "include_mapped",
                    "cohort_name")

    conceptList <- split(
      conceptList,
      conceptList[, c("cohort_name")]
    )

    for(j in seq_along(conceptList)){
      conceptList[[j]] <- conceptList[[j]] |>
        dplyr::select(!"cohort_name")
    }
    conceptList <- omopgenerics::newConceptSetExpression(conceptList)
    return(conceptList)
  }


  if(any(conceptList$include_mapped == TRUE)){
    exc <- paste(conceptList |>
                   dplyr::filter(.data$include_mapped == TRUE) |>
                   dplyr::pull("cohort_name"), collapse = "; ")
    cli::cli_abort(
      glue::glue("Mapped as TRUE not supported (found in {exc})"))
  }

  # second part: produce output list
  if(nrow(conceptList) == 0){
    cli::cli_abort("No concepts found in concept set")
  }

  conceptListTable <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = conceptListTable,
                                   table = conceptList,
                                   overwrite = TRUE,
                                   temporary = FALSE)

  conceptFinalList <- formatConceptList(cdm = cdm,
                                        conceptListTable = conceptListTable)

  if(type == "codelist"){
    conceptFinalList <- omopgenerics::newCodelist(conceptFinalList)
  }

  if(type == "codelist_with_details"){
    conceptFinalList <- addDetails(conceptList = conceptFinalList,
                                   cdm = cdm)

    x <- purrr::list_rbind(conceptFinalList) |>
      dplyr::filter(is.na(.data$standard_concept)) |>
      dplyr::pull("concept_id")
    if(length(x) > 0){
      cli::cli_warn("Concept_id{?s} {.val {as.character(x)}} {?is/are} not present in the CDM.")
    }
    conceptFinalList <- omopgenerics::newCodelistWithDetails(conceptFinalList)
  }

  # return list
  return(conceptFinalList)
}

#' Get concept ids from JSON files containing cohort definitions
#'
#' @param path Path to a file or folder containing JSONs of cohort definitions.
#' @inheritParams cdmDoc
#' @inheritParams typeBroadDoc
#'
#' @return Named list with concept_ids for each concept set.
#' @export
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' cdm <- mockVocabRef("database")
#' x <- codesFromCohort(cdm = cdm,
#'                      path =  system.file(package = "CodelistGenerator",
#'                      "cohorts_for_mock"))
#' x
#' CDMConnector::cdmDisconnect(cdm)
#' }
#'
codesFromCohort <- function(path,
                            cdm,
                            type = c("codelist")) {

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertCharacter(type, length = 1)
  omopgenerics::assertChoice(type, choices = c("codelist", "codelist_with_details", "concept_set_expression"))
  checkInputs(path = path, cdm = cdm)

  # list jsons
  files <- listJsonFromPath(path)

  # obtain codelistTibble
  codelistTibble <- NULL
  unknown <- 1
  for (k in seq_along(files)) {
    codelistTibble <- codelistTibble |>
      dplyr::union_all(extractCodes(files[k], unknown))
  }

  if(is.null(codelistTibble)){
    cli::cli_abort("No codes found")
  }

  if(type == "concept_set_expression"){
    codelistTibble <- codelistTibble |>
      dplyr::mutate(include_mapped = FALSE) |>
      dplyr::select("concept_id",
                    "excluded" = "is_excluded",
                    "descendants" = "include_descendants",
                    "mapped" = "include_mapped",
                    "codelist_name")

    codelistTibble <- split(
      codelistTibble,
      codelistTibble[, c("codelist_name")]
    )

    for(j in seq_along(codelistTibble)){
      codelistTibble[[j]] <- codelistTibble[[j]] |>
        dplyr::select(!"codelist_name")
    }
    codelistTibble <- omopgenerics::newConceptSetExpression(codelistTibble)
    return(codelistTibble)
  }


  codelistTable <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = codelistTable,
                                   table = codelistTibble,
                                   overwrite = TRUE,
                                   temporary = FALSE)

  # obtain descendants
  codelistTibble <- appendDescendants(cdm, codelistTable) |>
    dplyr::collect(name = codelistTable,
                   overwrite = TRUE,
                   temporary = FALSE)

  omopgenerics::dropSourceTable(cdm = cdm, name = codelistTable)
  cdm[[codelistTable]] <- NULL

  # exclude
  codelistTibble <- excludeCodes(codelistTibble)

  # split into list
  codelist <- tibbleToList(codelistTibble)

  if(type == "codelist"){
    codelist <- omopgenerics::newCodelist(codelist)
  }

  if(type == "codelist_with_details"){
    codelist <- addDetails(conceptList = codelist,
                           cdm = cdm)
    codelist <- omopgenerics::newCodelistWithDetails(codelist)
  }


  # return
  return(codelist)
}

listJsonFromPath <- function(path) {
  if (file.info(path)[["isdir"]]) {
    files <- list.files(path, full.names = TRUE)
  } else {
    files <- path
  }
  files <- files[tools::file_ext(files) == "json"]
  if (length(files) == 0) {
    cli::cli_abort(paste0("No json files find in ", path))
  }
  return(files)
}

extractCodes <- function(file, unknown) {

  json <- jsonlite::read_json(file)[["ConceptSets"]]
  codelistTibble <- NULL
  for (k in seq_along(json)) {
    name <- json[[k]][["name"]]
    if (is.null(name)) {
      name <- paste0("unkown concept set ", unknown)
      unknown <- unknown + 1
    }
    #else if (name %in% names(codes)) {
    #  basefile <- basename(file)
    #  name <- paste0(name, " (", substr(basefile, 1, nchar(basefile) - 5), ")")
    #}
    concepts <- json[[k]][["expression"]][["items"]]
    conceptId <- NULL
    includeDescendants <- NULL
    isExcluded <- NULL

    for (j in seq_along(concepts)) {
      if(!is.null(concepts[[j]][["includeMapped"]]) &&
         concepts[[j]][["includeMapped"]] == TRUE ){
        cli::cli_abort(
          glue::glue("Mapped as TRUE not supported (found in {name})"))
      }
      conceptId <- c(conceptId, concepts[[j]][["concept"]][["CONCEPT_ID"]])
      exc <- concepts[[j]][["isExcluded"]]
      isExcluded <- c(
        isExcluded, ifelse(is.null(exc), FALSE, exc)
      )
      incD <- concepts[[j]][["includeDescendants"]]
      includeDescendants <- c(
        includeDescendants, ifelse(is.null(incD), FALSE, incD)
      )
    }
    codelistTibble <- codelistTibble |>
      dplyr::union_all(dplyr::tibble(
        codelist_name = name, concept_id = conceptId,
        include_descendants = includeDescendants, is_excluded = isExcluded
      ) |>
        dplyr::mutate(filename = file))
  }
  return(codelistTibble)
}

appendDescendants <- function(cdm, codelistTable) {

  cdm[[codelistTable]] |>
    dplyr::mutate(concept_id = as.integer(.data$concept_id)) |>
    dplyr::filter(.data$include_descendants == TRUE) |>
    dplyr::rename("ancestor_concept_id" = "concept_id") |>
    dplyr::inner_join(
      cdm[["concept_ancestor"]] |>
        dplyr::select("ancestor_concept_id", "descendant_concept_id") |>
        dplyr::mutate(ancestor_concept_id = as.integer(.data$ancestor_concept_id)),
      by = "ancestor_concept_id"
    ) |>
    dplyr::select(-"ancestor_concept_id") |>
    dplyr::rename("concept_id" = "descendant_concept_id") |>
    dplyr::union_all(
      cdm[[codelistTable]]  |>
        dplyr::filter(.data$include_descendants == "FALSE")
    ) |>
    dplyr::select(-"include_descendants")

}

excludeCodes <- function(codelistTibble) {
  codelistTibble |>
    dplyr::filter(.data$is_excluded == FALSE) |>
    dplyr::select(-"is_excluded") |>
    dplyr::anti_join(
      codelistTibble |>
        dplyr::filter(.data$is_excluded == TRUE),
      by = c("codelist_name", "concept_id")
    )
}

tibbleToList <- function(codelistTibble) {

  codelistTibble <- codelistTibble |>
    dplyr::mutate(nam = paste0(.data$codelist_name, "; ",
                               .data$filename))

  nam <- unique(codelistTibble$nam)
  codelist <- lapply(nam, function(x) {
    codelistTibble |>
      dplyr::filter(.data$nam == .env$x) |>
      dplyr::pull("concept_id") |>
      unique()
  })
  names(codelist) <- nam


  # check if we have any concept sets with the same name but different definitions
  # keep first for each name
  cs_names <- stringr::str_extract(names(codelist), "^[^;]*")
  cs_names_unique <- unique(cs_names)

  codelist_dedup <- list()

  for(i in seq_along(cs_names_unique)){
    same_name_cs <- codelist[which(cs_names_unique[i] == cs_names)]
    check_consistent <- all(sapply(same_name_cs, identical, same_name_cs[[1]]))

    if(isFALSE(check_consistent)){
      cli::cli_abort(message = "Different definitions for concept set {cs_names_unique[i]} found")
    }
    # keep first
    codelist_dedup[[cs_names_unique[i]]] <- same_name_cs[[1]]
  }

  return(codelist_dedup)
}

addDetails <- function(conceptList,
                       cdm,
                       cols = c("concept_name", "domain_id", "vocabulary_id", "standard_concept")){

  if(length(conceptList) == 0){
    return(conceptList)
  }

  if(isFALSE(inherits(conceptList[[1]], "tbl_df"))){
    for(i in seq_along(conceptList)){
      conceptList[[i]] <- dplyr::tibble(concept_id = conceptList[[i]],
                                        concept_set = names(conceptList)[i])
    }
    extraCols <- as.character()
  }else{
    cols <- setdiff(cols,
                    colnames(conceptList[[1]]))
    conceptList <- purrr::imap(conceptList, ~ dplyr::mutate(.x, "concept_set" = .y))
    extraCols <- names(conceptList[[1]])[!names(conceptList[[1]]) %in% c("concept_id", "concept_name", "domain_id", "standard_concept", "vocabulary_id", "concept_set")]
  }

  conceptList <- dplyr::bind_rows(unclass(conceptList))

  tableConceptList <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   table = conceptList,
                                   name = tableConceptList,
                                   overwrite = TRUE,
                                   temporary = FALSE)

  if("standard_concept" %in% cols){
    conceptList <- cdm[[tableConceptList]] |>
      dplyr::left_join(cdm[["concept"]] |>
                         dplyr::select("concept_id", dplyr::all_of(cols)) |>
                         dplyr::mutate("standard_concept" = dplyr::if_else(
                           is.na(.data$standard_concept),
                           "non-standard",
                           .data$standard_concept)
                         ),
                       by = "concept_id")

    conceptList <- conceptList |>
      dplyr::mutate(
        standard_concept = ifelse(.data$standard_concept == "C",
                                  "classification",
                                  .data$standard_concept)) |>
      dplyr::mutate(
        standard_concept = ifelse(.data$standard_concept == "S",
                                  "standard",
                                  .data$standard_concept))  |>
      dplyr::collect()
  }else{
    conceptList <- cdm[[tableConceptList]] |>
      dplyr::left_join(cdm[["concept"]] |>
                         dplyr::select("concept_id", dplyr::all_of(cols)),
                       by = "concept_id")

    conceptList <- conceptList |>
      dplyr::collect()
  }

  omopgenerics::dropSourceTable(cdm = cdm, name = tableConceptList)

  # Arrange tables
  conceptList <- conceptList |>
    dplyr::select("concept_id", "concept_set",
                  dplyr::any_of(c("concept_name", "domain_id", "vocabulary_id", "standard_concept",
                           extraCols)))

  conceptList <- split(
    x = conceptList |> dplyr::select(!"concept_set"),
    f = as.factor(conceptList$concept_set)
  )

  for(i in seq_along(conceptList)){
    conceptList[[i]] <- conceptList[[i]] |>
      dplyr::arrange(.data$concept_id)
  }

  return(conceptList)
}

#' Put concept ids from all cohorts of interest in the required list format
#'
#' @param conceptList table with all the concept ids read, with their respective
#' cohort, exclusion and descendant information.
#' @inheritParams cdmDoc
#'
#' @return list of concept_ids and respective cohort_definition_ids of interest
#' @noRd
formatConceptList <- function(cdm, conceptListTable) {
  conceptList <- cdm[[conceptListTable]] |>
    dplyr::filter(.data$include_descendants == FALSE) |>
    dplyr::union_all(
      cdm[["concept_ancestor"]] |>
        dplyr::select(
          "concept_id" = "ancestor_concept_id",
          "descendant_concept_id"
        ) |>
        dplyr::mutate(concept_id = as.integer(.data$concept_id)) |>
        dplyr::right_join(
          cdm[[conceptListTable]] |>
            dplyr::mutate(concept_id = as.integer(.data$concept_id)) |>
            dplyr::filter(.data$include_descendants == TRUE),
          by = "concept_id"
        ) |>
        dplyr::mutate(descendant_concept_id = dplyr::if_else(
          # in case concept is not in ancestor table
          # (even though it should be)
          is.na(.data$descendant_concept_id),
          .data$concept_id, .data$descendant_concept_id)) |>
        dplyr::select(-"concept_id") |>
        dplyr::rename("concept_id" = "descendant_concept_id")
    ) |>
    dplyr::select(-"include_descendants") |>
    dplyr::collect()
  # eliminate the ones that is_excluded = TRUE
  conceptList <- conceptList |>
    dplyr::filter(.data$is_excluded == FALSE) |>
    dplyr::select("cohort_name", "concept_id") |>
    dplyr::anti_join(
      conceptList |>
        dplyr::filter(.data$is_excluded == TRUE),
      by = c("cohort_name","concept_id")
    )

  conceptFinalList <- list()
  for(n in conceptList[["cohort_name"]] |> unique()) {
    conceptFinalList[[n]] <- conceptList |>
      dplyr::filter(.data$cohort_name == n) |>
      dplyr::select("concept_id") |>
      dplyr::pull()
  }
  return(conceptFinalList)
}

#' Get concept ids and information from a list of json files provided
#'
#' @param conceptSets paths and names of all jsons to be read.
#'
#' @return raw table with all the concept_ids read, and their information
#' @noRd
readConceptSet <- function(conceptSets) {
  names <- c("CONCEPT_CLASS_ID",
             "CONCEPT_CODE",
             "CONCEPT_ID",
             "CONCEPT_NAME",
             "DOMAIN_ID",
             "INVALID_REASON",
             "INVALID_REASON_CAPTION",
             "STANDARD_CONCEPT",
             "STANDARD_CONCEPT_CAPTION",
             "VOCABULARY_ID",
             "isExcluded",
             "includeMapped",
             "includeDescendants")

  for (k in seq_len(nrow(conceptSets))) {
    conceptSetName <- conceptSets$concept_set_name[k]
    conceptSet <- jsonlite::read_json(conceptSets$concept_set_path[k])
    conceptSet <- lapply(conceptSet$items, function(x) {
      x <- append(x, x[["concept"]])
      x[["concept"]] <- NULL
      return(x)
    }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        cohort_name = conceptSetName
      )
    # Add columns missing from the read file with default values
    conceptSet[setdiff(names, names(conceptSet))] <- NA_character_
    conceptSet <- conceptSet |>
      dplyr::mutate(
        isExcluded = ifelse(is.na(.data$isExcluded), FALSE, .data$isExcluded),
        includeMapped = ifelse(
          is.na(.data$includeMapped), FALSE, .data$includeMapped
        ),
        includeDescendants = ifelse(
          is.na(.data$includeDescendants), FALSE, .data$includeDescendants
        )
      )

    if (k == 1) {
      conceptList <- conceptSet
    } else {
      conceptList <- dplyr::bind_rows(conceptList, conceptSet)
    }
  }
  conceptList <- conceptList |>
    dplyr::select(
      "cohort_name",
      "concept_id" = "CONCEPT_ID",
      "is_excluded" = "isExcluded",
      "include_descendants" = "includeDescendants",
      "include_mapped" = "includeMapped"
    )

  return(conceptList)
}
