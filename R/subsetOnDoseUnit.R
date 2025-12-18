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

#' Subset a codelist to only those with a particular dose unit.
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams doseUnitDoc
#' @param negate If FALSE, only concepts with the dose unit specified will be
#' returned. If TRUE, concepts with the dose unit specified will be excluded.
#'
#' @return The codelist with only those concepts associated with the
#' dose unit (if negate = FALSE) or codelist without those concepts associated with the
#' dose unit(if negate = TRUE).
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omopgenerics)
#' cdm <- mockVocabRef()
#' codes <- subsetOnDoseUnit(x = newCodelist(list("codes" = c(20,21))),
#'                           cdm = cdm,
#'                           doseUnit = c("milligram"))
#'
#' codes
#' }
subsetOnDoseUnit <- function(x,
                             cdm,
                             doseUnit,
                             negate = FALSE){

  omopgenerics::assertCharacter(doseUnit, null = FALSE, na = FALSE)

  x <- subsetCodelistBy(x,
                        cdm,
                        by = "dose_unit",
                        group = doseUnit,
                        keepOriginal = FALSE,
                        negate = negate)
  return(x)
}
