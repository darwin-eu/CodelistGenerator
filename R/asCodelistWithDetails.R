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

#' Coerce to a codelist with details
#'
#' @param x Only codelist and candidate_codes are currently supported.
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
#' path <- downloadMockDataset(datasetName = "GiBleed",
#'                             path = NULL,
#'                             overwrite = NULL)
#' cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#'
#' # Create codelist_with_details from a codelist
#' codelist <- getDrugIngredientCodes(cdm,
#'                                    name = "acetaminophen",
#'                                    nameStyle = "{concept_name}",
#'                                    type = "codelist")
#'
#' asCodelistWithDetails(codelist, cdm)
#'
#' # Create codelist from a candidate_codes
#' codelist <- getCandidateCodes(cdm,
#'                              keywords = "arthritis")
#'
#' asCodelistWithDetails(codelist)
#' }
asCodelistWithDetails <- function(x, cdm, ...){
  UseMethod("asCodelistWithDetails")
}

#' @export
#' @rdname asCodelistWithDetails
asCodelistWithDetails.codelist_with_details  <- function(x, ...){

  x |>
    omopgenerics::newCodelistWithDetails()

}

#' @export
#' @rdname asCodelistWithDetails
asCodelistWithDetails.codelist  <- function(x, cdm, ...){

  addDetails(x, cdm = cdm) |>
    omopgenerics::newCodelistWithDetails()

}

#' @export
#' @rdname asCodelistWithDetails
asCodelistWithDetails.candidate_codes  <- function(x, cdm, ...){
  x <- removeClass(x, "candidate_codes")
  list("candidate_codes" = x |>
         dplyr::select("concept_id", "concept_name",
                       "domain_id", "vocabulary_id", "standard_concept")
  ) |>
    omopgenerics::newCodelistWithDetails()
}
