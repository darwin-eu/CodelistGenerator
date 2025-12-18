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


#' Compare overlap between two sets of codes
#'
#' @param codelist1 Output of getCandidateCodes or a codelist
#' @param codelist2 Output of getCandidateCodes.
#'
#' @return Tibble with information on the overlap of codes in both codelists.
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omock)
#'
#' # Create a CDM object
#' downloadMockDataset(datasetName = "GiBleed",
#'                     path = NULL,
#'                     overwrite = NULL)
#' cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#'
#' # Compare two candidate_codes object
#' codes1 <- getCandidateCodes(
#'   cdm = cdm,
#'   keywords = "Arthritis",
#'   domains = "Condition",
#'   includeDescendants = TRUE)
#'
#' codes2 <- getCandidateCodes(
#'   cdm = cdm,
#'   keywords = c("osteo"),
#'   domains = "Condition",
#'   includeDescendants = TRUE)
#'
#' compareCodelists(
#'   codelist1 = codes1,
#'   codelist2 = codes2)
#'
#' # Compare two codelists
#' acetaminophen <- getDrugIngredientCodes(cdm,
#'                                         name = "acetaminophen",
#'                                         nameStyle = "{concept_name}",
#'                                         type = "codelist")
#'
#' hydrocodone <- getDrugIngredientCodes(cdm,
#'                                       name = "hydrocodone",
#'                                       nameStyle = "{concept_name}",
#'                                       type = "codelist")
#' compareCodelists(
#'   codelist1 = acetaminophen,
#'   codelist2 = hydrocodone)
#' # Notice that concept_name = NA as `codelist` class does not store this information
#' # for each concept.
#'
#' # Compare two codelists_with_details
#' acetaminophen <- getDrugIngredientCodes(cdm,
#'                                         name = "acetaminophen",
#'                                         nameStyle = "{concept_name}",
#'                                         type = "codelist_with_details")
#'
#' hydrocodone <- getDrugIngredientCodes(cdm,
#'                                       name = "hydrocodone",
#'                                       nameStyle = "{concept_name}",
#'                                       type = "codelist_with_details")
#' compareCodelists(
#'   codelist1 = acetaminophen,
#'   codelist2 = hydrocodone)
#'
#' }
compareCodelists <- function(codelist1,
                             codelist2) {

  if(!inherits(codelist1, "candidate_codes")){
    checkCodelist(codelist1, allowConceptSetExpression = FALSE)
  }
  if(!inherits(codelist2, "candidate_codes")){
    checkCodelist(codelist2, allowConceptSetExpression = FALSE)
  }

  codelist1 <- convertCodelist(codelist1, name = "codelist1")
  codelist2 <- convertCodelist(codelist2, name = "codelist2")

  codes <- list()
  for(i in seq_along(codelist1)){
    for(j in seq_along(codelist2)){
      workingName <- paste0(names(codelist1)[[i]],"_", names(codelist2)[[j]])

      codes[[workingName]] <- dplyr::full_join(codelist1[[i]] |>
                                                 dplyr::select("concept_id", "concept_name") |>
                                                 dplyr::mutate(codelist_1 = 1) |>
                                                 dplyr::distinct(),
                                               codelist2[[j]] |>
                                                 dplyr::select("concept_id", "concept_name") |>
                                                 dplyr::mutate(codelist_2 = 1) |>
                                                 dplyr::distinct(),
                                               by = c("concept_id", "concept_name"))

      codes[[workingName]] <- codes[[workingName]] |>
        dplyr::mutate(
          "codelist" = dplyr::case_when(
            !is.na(codelist_1) & is.na(codelist_2) ~ paste0("Only in codelist ", names(codelist1)[[i]]),
            is.na(codelist_1) & !is.na(codelist_2) ~  paste0("Only in codelist ", names(codelist2)[[j]]),
            !is.na(codelist_1) & !is.na(codelist_2) ~ "Both")) |>
        dplyr::select(-c("codelist_1", "codelist_2"))
    }
  }

  if(length(codes) == 1){
    codes <- codes[[1]] |>
      dplyr::arrange(.data$concept_id)
  }
  return(codes)
}


convertCodelist <- function(codelist, name){
  if(inherits(codelist, "codelist")){
    codelist <- purrr::map(
      codelist,
      ~dplyr::tibble("concept_id" = .x,
                     "concept_name" = NA_character_)
    )
  }

  if(inherits(codelist, "codelist_with_details")){
    if("concept_name" %in% colnames(codelist[[1]])){
      codelist <- purrr::map(
        codelist,
        ~dplyr::tibble("concept_id" = dplyr::pull(.x, "concept_id"),
                       "concept_name" = dplyr::pull(.x, "concept_name"))
      )
    }else{
      codelist <- purrr::map(
        codelist,
        ~dplyr::tibble("concept_id" = dplyr::pull(.x, "concept_id"),
                       "concept_name" = NA_character_)

      )
    }
  }

  if(inherits(codelist, "candidate_codes")){
    codelist <- removeClass(codelist, "candidate_codes")
    codelist <- list("codelist" = codelist |>
                       dplyr::select("concept_id", "concept_name"))
    names(codelist) <- name
  }

  return(codelist)
}
