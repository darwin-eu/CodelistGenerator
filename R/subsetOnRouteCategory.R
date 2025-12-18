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

#' Subset a codelist to only those with a particular route category
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams routeCategoryDoc
#' @param negate If FALSE, only concepts with the routeCategory specified will
#' be returned. If TRUE, concepts with the routeCategory specified will be excluded.
#'
#' @return The codelist with only those concepts associated with the
#' specified route categories (if negate is FALSE) or the codelist without those
#' concepts associated with the specified route categories (if negate is TRUE).
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omopgenerics)
#' cdm <- mockVocabRef()
#' codes <- subsetOnRouteCategory(
#'               x = newCodelist(list("codes" = c(20,21))),
#'               cdm = cdm,
#'               routeCategory = "topical")
#' codes
#' }
subsetOnRouteCategory <- function(x,
                                  cdm,
                                  routeCategory,
                                  negate = FALSE){

  omopgenerics::assertCharacter(routeCategory, null = FALSE, na = FALSE)

  x <- subsetCodelistBy(x,
                        cdm,
                        by = "route_category",
                        group = routeCategory,
                        keepOriginal = FALSE,
                        negate = negate)
  return(x)
}
