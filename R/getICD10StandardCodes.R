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

#' Get corresponding standard codes for International Classification of Diseases (ICD) 10 codes
#'
#' @inheritParams cdmDoc
#' @inheritParams levelICD10Doc
#' @param name Name of chapter or sub-chapter of interest. If NULL, all
#' will be considered.
#' @inheritParams nameStyleDoc
#' @inheritParams includeDescendantsDoc
#' @inheritParams typeNarrowDoc
#'
#' @return A named list, with each element containing the corresponding
#' standard codes (and descendants) of ICD chapters and sub-chapters.
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' cdm <- mockVocabRef()
#' getICD10StandardCodes(cdm = cdm, level = c(
#'   "ICD10 Chapter",
#'   "ICD10 SubChapter"
#' ))
#' }
getICD10StandardCodes <- function(cdm,
                                  level = c("ICD10 Chapter","ICD10 SubChapter"),
                                  name = NULL,
                                  nameStyle = "{concept_code}_{concept_name}",
                                  includeDescendants = TRUE,
                                  type = "codelist") {

  if(type == "concept_set_expression"){
    cli::cli_abort("concept_set_expression not yet supported")
  }

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertChoice(level, c(
    "ICD10 Chapter",
    "ICD10 SubChapter",
    "ICD10 Hierarchy",
    "ICD10 Code"
  ))
  omopgenerics::assertCharacter(name, null = T)
  omopgenerics::assertLogical(includeDescendants)
  omopgenerics::assertChoice(type, c(
    "codelist",
    "codelist_with_details",
    "concept_set_expression"))
  nameStyle <- checkNameStyle(nameStyle)

  # first get non standard codes
  cli::cli_inform("Getting non-standard ICD10 concepts")
  ICD10NonStandardCodes <- getICD10NonStandardCodes(
    cdm = cdm,
    level = level,
    name = name,
    nameStyle = nameStyle
  )

  if (!length(ICD10NonStandardCodes) > 0) {
    cli::cli_h2(
      c(
        "i" = "No codes found"
      )
    )
    cli::cli_inform(
      c(
        "i" = "No ICD-10 chapter codes were found in the concept table.",
        "--  Were ICD-10 codes included when loading vocabularies into the database?"
      )
    )
    return(dplyr::tibble())
  }

  # map from non-standard to standard and add descendants
  cli::cli_inform("Mapping from non-standard to standard concepts")
  for (i in seq_along(ICD10NonStandardCodes)) {
    ICD10NonStandardCodes[[i]] <- dplyr::tibble(
      concept_id = ICD10NonStandardCodes[[i]],
      name = names(ICD10NonStandardCodes)[i]
    )
  }
  ICD10NonStandardCodes <- dplyr::bind_rows(ICD10NonStandardCodes)
  # map to standard
  tmpTblName1 <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = tmpTblName1,
                                   table = ICD10NonStandardCodes,
                                   overwrite = TRUE,
                                   temporary = FALSE)
  cdm[[tmpTblName1]] <- cdm[[tmpTblName1]] |>
    dplyr::inner_join(
      cdm[["concept_relationship"]] |>
        dplyr::filter(.data$relationship_id == "Maps to"),
      by = c("concept_id" = "concept_id_1"),
      relationship = "many-to-many"
    ) |>
    dplyr::select("concept_id_2", "name") |>
    dplyr::rename("concept_id" = "concept_id_2")

  ICD10MapsTo <- cdm$concept |>
    dplyr::select("concept_id") |>
    dplyr::inner_join(cdm[[tmpTblName1]],
      by = "concept_id"
    ) |>
    dplyr::mutate("parent_concept_id" = .data$concept_id) |>
    dplyr::distinct()

  if(!is.null(attr(cdm, "dbcon"))){
    ICD10MapsTo <- ICD10MapsTo |>
      dplyr::compute()
  }

  # add descendants
  if (isTRUE(includeDescendants)) {
    cli::cli_inform("Getting descendant concepts")
    ICD10MapsTo <- ICD10MapsTo |>
      dplyr::left_join(cdm$concept_ancestor,
        by = c("concept_id" = "ancestor_concept_id")
      ) |>
      dplyr::select("name",
                    "parent_concept_id",
                    "concept_id" = "descendant_concept_id")

    if(!is.null(attr(cdm, "dbcon"))){
      ICD10MapsTo <- ICD10MapsTo  |>
      dplyr::compute()
      }
  }

  if(type == "codelist_with_details") {
    ICD10MapsTo <- ICD10MapsTo |>
      dplyr::left_join(cdm[["concept"]] |>
                         dplyr::select("concept_id", "concept_code", "concept_name",
                                       "domain_id", "vocabulary_id"),
                       by = "concept_id") |>
      dplyr::select(-"parent_concept_id")

    # split into list
    ICD10StandardCodes <- ICD10MapsTo |>
      dplyr::collect()

    ICD10StandardCodes <- split(
      x = ICD10StandardCodes,
      f = as.factor(ICD10StandardCodes$name),
      drop = TRUE
    ) |>
      omopgenerics::newCodelistWithDetails()
  } else if(type == "codelist"){
    # split into list (only returning vector of concept ids)
    ICD10StandardCodes <- ICD10MapsTo |>
      dplyr::collect()
    ICD10StandardCodes <- split(
      x = ICD10StandardCodes$concept_id,
      f = ICD10StandardCodes$name
    ) |>
      omopgenerics::newCodelist()
  }

  omopgenerics::dropSourceTable(cdm = cdm, name = tmpTblName1)

  return(ICD10StandardCodes)
}


# get non-standard descendant codes for icd chapters and sub-chapters
getICD10NonStandardCodes <- function(cdm,
                                     level = c(
                                       "ICD10 Chapter",
                                       "ICD10 SubChapter",
                                       "ICD10 Hierarchy",
                                       "ICD10 Code"),
                                     name,
                                     nameStyle) {

  if(!is.null(name)){
    conceptDb <- cdm[["concept"]] |>
      dplyr::filter(tolower(.data$concept_name) == tolower(.env$name))
  } else {
    conceptDb <- cdm[["concept"]]
  }

  if ("ICD10 SubChapter" %in% level) {
    # go down two levels to get specific codes
    icd_sub <- conceptDb |>
      dplyr::filter(.data$vocabulary_id == "ICD10",
                    .data$concept_class_id %in% "ICD10 SubChapter") |>
      dplyr::select("concept_id", "concept_name", "concept_code")
    if(!is.null(attr(cdm, "dbcon"))){
      icd_sub <- icd_sub |>
      dplyr::compute()
    }

    icd_sub1 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_sub |>
        dplyr::rename(
          "concept_id_1" =
            "concept_id"
        )
    )
    if(!is.null(attr(cdm, "dbcon"))){
      icd_sub1 <- icd_sub1  |>
      dplyr::compute()
      }
    # one more level down
    icd_sub2 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_sub1
    )

    if(!is.null(attr(cdm, "dbcon"))){
      icd_sub2 <-  icd_sub2 |>
      dplyr::compute()}

    icd_subchapter <- icd_sub2 |>
      dplyr::collect() |>
      dplyr::mutate(name = tidyWords(.data$concept_name)) |>
      dplyr::mutate(name = stringr::str_replace_all(.data$name, " ", "_")) |>
      dplyr::mutate(concept_code = tidyWords(.data$concept_code)) |>
      dplyr::mutate(concept_code = stringr::str_replace_all(.data$concept_code, " ", "_")) |>
      dplyr::select("concept_id_1", "name",
                    "concept_code") |>
      dplyr::distinct()
  } else {
    icd_subchapter <- dplyr::tibble()
  }

  if ("ICD10 Chapter" %in% level) {
    # go down three levels to get specific codes
    icd_ch <- conceptDb |>
      dplyr::filter(.data$vocabulary_id == "ICD10",
                    .data$concept_class_id %in% "ICD10 Chapter") |>
      dplyr::select("concept_id", "concept_name", "concept_code")
    if(!is.null(attr(cdm, "dbcon"))){
      icd_ch <-icd_ch  |>
      dplyr::compute()
      }

    icd_ch1 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_ch |>
        dplyr::rename(
          "concept_id_1" =
            "concept_id"
        )
    )
    if(!is.null(attr(cdm, "dbcon"))){
      icd_ch1 <- icd_ch1 |>
      dplyr::compute()
    }
    # one more level down
    icd_ch2 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_ch1
    )
    if(!is.null(attr(cdm, "dbcon"))){
      icd_ch2 <- icd_ch2 |>
      dplyr::compute()
    }
    # and one more level down
    icd_ch3 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_ch2
    )
    if(!is.null(attr(cdm, "dbcon"))){
      icd_ch3 <-icd_ch3 |>
      dplyr::compute()}
    icd_chapter <- icd_ch3 |>
      dplyr::collect() |>
      dplyr::mutate(name = tidyWords(.data$concept_name)) |>
      dplyr::mutate(name = stringr::str_replace_all(.data$name, " ", "_")) |>
      dplyr::mutate(concept_code = tidyWords(.data$concept_code)) |>
      dplyr::mutate(concept_code = stringr::str_replace_all(.data$concept_code, " ", "_")) |>
      dplyr::select("concept_id_1", "name", "concept_code") |>
      dplyr::distinct()
  } else {
    icd_chapter <- dplyr::tibble()
  }

  if ("ICD10 Hierarchy" %in% level) {
    # go down one level to get specific codes
    icd_sub <- conceptDb |>
      dplyr::filter(.data$vocabulary_id == "ICD10",
                    .data$concept_class_id %in% "ICD10 Hierarchy") |>
      dplyr::select("concept_id", "concept_name", "concept_code")
    if(!is.null(attr(cdm, "dbcon"))){
      icd_sub <- icd_sub |>
        dplyr::compute()
    }

    icd_sub1 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_sub |>
        dplyr::rename(
          "concept_id_1" =
            "concept_id"
        )
    )
    if(!is.null(attr(cdm, "dbcon"))){
      icd_sub1 <- icd_sub1  |>
        dplyr::compute()
    }
    # one more level down
    icd_hierarchy <- icd_sub1 |>
      dplyr::collect() |>
      dplyr::mutate(name = tidyWords(.data$concept_name)) |>
      dplyr::mutate(name = stringr::str_replace_all(.data$name, " ", "_")) |>
      dplyr::mutate(concept_code = tidyWords(.data$concept_code)) |>
      dplyr::mutate(concept_code = stringr::str_replace_all(.data$concept_code, " ", "_")) |>
      dplyr::select("concept_id_1", "name",
                    "concept_code") |>
      dplyr::distinct()
  } else {
    icd_hierarchy <- dplyr::tibble()
  }

  if ("ICD10 Code" %in% level) {
    # same level
    icd_code <- conceptDb |>
      dplyr::filter(.data$vocabulary_id == "ICD10",
                    .data$concept_class_id %in% c("ICD10 Code", "ICD10 code")) |>
      dplyr::select("concept_id", "concept_name", "concept_code") |>
      dplyr::collect() |>
      dplyr::mutate(name = tidyWords(.data$concept_name)) |>
      dplyr::mutate(name = stringr::str_replace_all(.data$name, " ", "_")) |>
      dplyr::mutate(concept_code = tidyWords(.data$concept_code)) |>
      dplyr::mutate(concept_code = stringr::str_replace_all(.data$concept_code, " ", "_")) |>
      dplyr::select("concept_id_1" = "concept_id",
                    "name",
                    "concept_code") |>
      dplyr::distinct()
  } else {
    icd_code <- dplyr::tibble()
  }

  icd <- dplyr::bind_rows(icd_chapter, icd_subchapter, icd_hierarchy, icd_code) |>
    dplyr::rename("concept_id" = "concept_id_1",
                  "concept_name" = "name")
  icd <- icd |>
    dplyr::mutate("name_styled" = glue::glue(.env$nameStyle))

  ICDNonStandardCodes <- split(
    x = icd$concept_id,
    f = icd$name_styled
  )


  return(ICDNonStandardCodes)
}

# get the subsumed concepts
# and return switching from concept_id_2 to concept_id_1 (so that we can
# run again to get next set of subsumed)
get_subsumed_concepts <- function(cdm,
                                  concepts) {
  concepts |>
    dplyr::inner_join(cdm[["concept_relationship"]],
      by = "concept_id_1"
    ) |>
    dplyr::filter(.data$relationship_id == "Subsumes") |>
    dplyr::select("concept_id_2", "concept_name", "concept_code") |>
    dplyr::rename("concept_id_1" = "concept_id_2")
}


checkNameStyle <- function(nameStyle){
  x <- nameStyle %in% c("{concept_code}", "{concept_id}", "{concept_name}",
                        "{concept_code}_{concept_id}", "{concept_code}_{concept_name}", "{concept_id}_{concept_code}",
                        "{concept_id}_{concept_name}", "{concept_name}_{concept_code}", "{concept_name}_{concept_id}",
                        "{concept_code}_{concept_id}_{concept_name}", "{concept_code}_{concept_name}_{concept_id}",
                        "{concept_id}_{concept_code}_{concept_name}", "{concept_id}_{concept_name}_{concept_code}",
                        "{concept_name}_{concept_code}_{concept_id}", "{concept_name}_{concept_id}_{concept_code}")
  if(isFALSE(x)){
    cli::cli_abort("nameStyle {nameStyle} is not supported. nameStyle argument must be a choice between {{concept_name}}, {{concept_id}}, or {{concept_code}},
                   or any combination of the three separated by `_` (i.e., {{concept_id}}_{{concept_name}}.")}

  return(nameStyle)
}
