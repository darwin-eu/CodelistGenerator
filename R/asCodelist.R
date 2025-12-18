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

#' Coerce to a codelist
#'
#' @param x Only codelist_with_details and candidate_codes are currently supported.
#' @inheritParams cdmDoc
#' @param ... For extensibility
#'
#' @returns codelist
#' @export
#'
#' @examples
#' \donttest{
#' library(omock)
#' library(CDMConnector)
#'
#' # Creating CDM object
#' cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#'
#' # Create codelist from a codelist_with_details
#' codelist <- getDrugIngredientCodes(cdm,
#'                                    name = "acetaminophen",
#'                                    nameStyle = "{concept_name}",
#'                                    type = "codelist_with_details")
#'
#' asCodelist(codelist)
#'
#' # Create codelist from a candidate_codes
#' codelist <- getCandidateCodes(cdm,
#'                               keywords = "arthritis")
#'
#' asCodelist(codelist)
#'
#' }
asCodelist <- function(x, ...){
  UseMethod("asCodelist")
}

#' @export
#' @rdname asCodelist
asCodelist.codelist <- function(x, ...){
  x |>
    newCodelist()
}

#' @export
#' @rdname asCodelist
asCodelist.codelist_with_details  <- function(x, ...){
  x |>
    purrr::map(~dplyr::pull(., "concept_id")) |>
    newCodelist()
}

#' @export
#' @rdname asCodelist
asCodelist.concept_set_expression  <- function(x, cdm, ...){

 # don't support mapped
 hasMapped <- x |>
    purrr::keep(~ any(.x$mapped, na.rm = TRUE)) |>
    names()
 if (length(hasMapped) > 0) {
  cli::cli_abort(
    glue::glue("Mapped as TRUE not supported (found in {hasMapped})"))
 }

 omopgenerics::validateConceptSetArgument(x, cdm = cdm)
}

#' @export
#' @rdname asCodelist
asCodelist.candidate_codes  <- function(x, ...){
  list("candidate_codes" = x |>
         dplyr::pull("concept_id")) |>
    newCodelist()
}
