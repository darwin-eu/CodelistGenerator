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

#' Report the search strategy used to identify codes when using the
#' `getCandidateCodes()` function
#'
#' @inheritParams xDoc
#'
#' @return A tibble with the search strategy
#' @export
#' @examples
#' \donttest{
#' library(omock)
#' library(CodelistGenerator)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # Create CDM object
#' cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#' codes <- getCandidateCodes(cdm = cdm,
#'                            keywords = c("sprain", "fracture"),
#'                            exclude = "knee",
#'                            domains = "Condition",
#'                            standardConcept = "Standard",
#'                            searchNonStandard = FALSE,
#'                            searchInSynonyms = TRUE,
#'                            includeDescendants = TRUE,
#'                            includeAncestor = FALSE)
#'
#' searchStrategy(codes) |>
#'     glimpse()
#' }
searchStrategy <- function(x){
  if(!inherits(x, "candidate_codes")){
    cli::cli_abort("`x` must have class `candidate_codes`.")
  }
  attr(x, "search_strategy")
}
