# Copyright 2024 DARWIN EU (C)
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

#' Subset a codelist to only those codes from a particular domain.
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @param doseForm Dose form/s. See `availableDoseForms()` to explore available dose forms
#' in your codelist.
#' @param negate If FALSE, only concepts with the dose form specified will be
#' returned. If TRUE, concepts with the dose form specified will be excluded.
#'
#' @return The codelist with only those concepts associated with the dose form
#' (if negate = FALSE) or the codelist without those concepts associated with
#' the dose form (if negate = TRUE).
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omopgenerics)
#' cdm <- mockVocabRef()
#'
#' codelist <- newCodelist(list("codes" = c(10L,20L,21L)))
#'
#' # Dose forms present in our codelist:
#' codelist |> associatedDoseForms(cdm)
#'
#' codes <- subsetOnDoseForm(
#'               x = codelist,
#'               cdm = cdm,
#'               doseForm = "Injection")
#' codes
#'
#' codes |> associatedDoseForms(cdm)
#' }
#'

subsetOnDoseForm <- function(x, cdm, doseForm, negate = FALSE){

  # Initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm)
  omopgenerics::assertCharacter(doseForm)
  checkCodelist(x, allowConceptSetExpression = FALSE)
  omopgenerics::assertLogical(negate, length = 1)

  x_original <- x
  if(inherits(x_original, "codelist_with_details")){
    x <- asCodelist(x)
    newX <- omopgenerics::emptyCodelistWithDetails()
  }
  if(inherits(x_original, "codelist")){
    newX <- omopgenerics::emptyCodelist()
  }

  tableCodelist <- paste0(omopgenerics::uniqueTableName(),
                          omopgenerics::uniqueId())

  for(i in seq_along(x)){
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = tableCodelist,
                                     table = dplyr::tibble(concept_id = x[[i]]),
                                     overwrite = TRUE,
                                     temporary = FALSE)

    cdm[[tableCodelist]] <- cdm[[tableCodelist]] |>
      dplyr::left_join(cdm[["concept_relationship"]] |>
                          dplyr::filter(.data$relationship_id == "RxNorm has dose form"),
                        by = c("concept_id" = "concept_id_1")) |>
      dplyr::select("concept_id", "dose_form_concept" = "concept_id_2") |>
      dplyr::distinct() |>
      dplyr::compute(temporary = FALSE, name = tableCodelist)

    x[[i]] <- cdm[[tableCodelist]] |>
      dplyr::left_join(
        cdm[["concept"]] |>
          dplyr::rename("dose_form_concept" = "concept_id"),
        by = c("dose_form_concept")
      ) |>
      dplyr::select("concept_id", "dose_form" = "concept_name") |>
      dplyr::distinct() |>
      dplyr::collect()

    if(isTRUE(negate)){
      x[[i]] <- x[[i]] |>
        dplyr::filter(!tolower(.data$dose_form) %in% tolower(.env$doseForm))
    }else{
      x[[i]] <- x[[i]] |>
        dplyr::filter(tolower(.data$dose_form) %in% tolower(.env$doseForm))
    }

    x[[i]] <- x[[i]] |>
      dplyr::pull("concept_id") |>
      unique() |>
      sort()

    if(inherits(x_original, "codelist_with_details")){
      newX[[names(x_original)[[i]]]] <- x_original[[i]] |>
        dplyr::inner_join(
          dplyr::tibble("concept_id" = x[[i]]),
          by = "concept_id"
        )
    }
    if(inherits(x_original, "codelist")){
      newX[[names(x_original)[[i]]]] <- x[[i]]
    }
  }

  newX <- dropEmptyCodelist(x_original, newX)

  cdm <- omopgenerics::dropSourceTable(cdm = cdm, name = tableCodelist)

  return(newX)
}
