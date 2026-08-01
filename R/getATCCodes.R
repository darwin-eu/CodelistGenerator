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

#' Get the descendant codes of Anatomical Therapeutic Chemical (ATC) classification codes
#'
#' @inheritParams cdmDoc
#' @inheritParams levelATCDoc
#' @param name ATC name of interest. For example, c("Dermatologicals",
#' "Nervous System"), would result in a list of length two with the descendant
#' concepts for these two particular ATC groups.
#' @inheritParams nameStyleDoc
#' @inheritParams doseFormDoc
#' @inheritParams doseUnitDoc
#' @inheritParams routeCategoryDoc
#' @inheritParams typeNarrowDoc
#'
#' @return Concepts with their format based on the type argument
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omock)
#'
#' # Create CDM object
#' cdm <- mockCdmReference()
#'
#' # Create a codelist with 1st level ATC codes available in the CDM
#' codelist <- getATCCodes(cdm = cdm,
#'                         level = "ATC 1st")
#' codelist
#'
#' # Tune the name of the generated codelists
#' codelist <- getATCCodes(cdm = cdm,
#'                         level = "ATC 1st",
#'                         nameStyle = "{concept_name}_{concept_code}")
#' codelist
#'
#' # Search for a specific ATC name of interest
#' codelist <- getATCCodes(cdm = cdm,
#'                         level = "ATC 2nd",
#'                         name = "immunostimulants")
#' codelist
#'
#' # Restrict concepts to specific dose forms, dose units, or route categories.
#' # Remember that you can use `availableDoseForm()`, `availableDoseUnit()` and
#' # `availableRouteCategory()` to explore your codelist.
#' codelist <- getATCCodes(cdm = cdm,
#'                         level = "ATC 2nd",
#'                         doseForm = NULL,
#'                         doseUnit = NULL,
#'                         routeCategory = NULL,)
#' codelist
#'
#' # You can also create directly a codelist_with_details using the argument `type`
#' codelist <- getATCCodes(cdm = cdm,
#'                         level = "ATC 1st",
#'                         type = "codelist_with_details")
#' codelist
#' }
getATCCodes <- function(cdm,
                        level = c("ATC 1st"),
                        name = NULL,
                        nameStyle = "{concept_code}_{concept_name}",
                        doseForm = NULL,
                        doseUnit = NULL,
                        routeCategory = NULL,
                        type = "codelist") {

  # initial checks
  if(type == "concept_set_expression"){
    cli::cli_abort("concept_set_expression not yet supported")
  }
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertChoice(level, c("ATC 1st", "ATC 2nd", "ATC 3rd", "ATC 4th", "ATC 5th"))
  omopgenerics::assertCharacter(name, null = TRUE)
  nameStyle <- checkNameStyle(nameStyle)
  omopgenerics::assertCharacter(doseForm, null = TRUE)
  omopgenerics::assertCharacter(doseUnit, null = TRUE)
  omopgenerics::assertCharacter(routeCategory, null = TRUE)
  type <- validateType(type = type)

  searchStrategy <- searchStrategyAttr(
    function_name = "getATCCodes",
    cdm = "cdm",
    level = cast(level),
    name = cast(name),
    doseForm = cast(doseForm)
  )

  atc_groups <- cdm$concept |>
    dplyr::filter(.data$vocabulary_id == "ATC") |>
    dplyr::filter(.data$concept_class_id %in% .env$level) |>
    dplyr::select("concept_id", "concept_name", "concept_code") |>
    dplyr::collect()

  if (!is.null(name)) {
    atc_groups <- atc_groups |>
      dplyr::filter(tidyWords(.data$concept_name) %in% tidyWords(.env$name))
  }

  if (nrow(atc_groups) > 0) {
    if (nrow(atc_groups) > 1 & type == "code_search") {
      cli::cli_abort(c(x = "Only one ATC type is supported when type = {.cls code_search}"))
    }
    # to avoid potential memory problems will batch
    atc_descendants <- fetchBatchedDescendants(cdm = cdm,
                                               codes = atc_groups$concept_id,
                                               batchSize = 500,
                                               doseForm = doseForm)
  } else {
    cli::cli_inform(c("!" = "- No matching ATC codes found"))
    return(emptyObject(type = type, searchStrategy = searchStrategy))
  }

  atc_descendants <- atc_descendants |>
    dplyr::select(!dplyr::any_of(c("min_levels_of_separation", "max_levels_of_separation"))) |>
    # split different ancestors into multiple cols
    tidyr::separate_wider_delim(
      cols = "ancestor_concept_id",
      delim = ";",
      names_sep = "",
      too_few = "align_start"
    )

  atc_descendants <- atc_descendants |>
    # one row per concept + ancestor
    tidyr::pivot_longer(cols = dplyr::starts_with("ancestor_concept_id"),
                        names_to = NULL,
                        values_to = "ancestor_concept_id",
                        values_drop_na = TRUE
    )

  atc_descendants <- atc_descendants |>
    dplyr::left_join(
      atc_groups |>
        dplyr::mutate("concept_name" = omopgenerics::toSnakeCase(.data$concept_name),
                      "concept_id" = as.character(.data$concept_id)) |>
        dplyr::mutate("name" = glue::glue(.env$nameStyle)) |>
        dplyr::select("concept_id", "name"),
      by = c("ancestor_concept_id" = "concept_id")
    )

  if(type == "codelist"){
    atc_descendants <- split(
      x = atc_descendants$concept_id,
      f = as.factor(atc_descendants$name),
      drop = TRUE
    ) |>
      omopgenerics::newCodelist()
  }else if(type == "codelist_with_details"){
    atc_descendants <- split(
      x = atc_descendants,
      f = as.factor(atc_descendants$name),
      drop = TRUE
    ) |>
      purrr::map(~dplyr::select(., -"name")) |>
      omopgenerics::newCodelistWithDetails()
  } else if (type == "code_search") {
    nm <- unique(atc_descendants$name)
    atc_descendants <- atc_descendants |>
      dplyr::select(!c("name", "ancestor_concept_id")) |>
      dplyr::mutate(
        found_from = "Descendants",
        vocabulary_version = vocabularyVersion(cdm = cdm),
      ) |>
      newCodeSearch(searchStrategy = searchStrategy)
  }


  if(!is.null(routeCategory)){
    atc_descendants <- subsetOnRouteCategory(atc_descendants,
                                             cdm = cdm,
                                             routeCategory = routeCategory)
  }

  if(!is.null(doseUnit)){
    atc_descendants <- subsetOnDoseUnit(atc_descendants,
                                        cdm = cdm,
                                        doseUnit = doseUnit)
  }

  return(atc_descendants)
}

