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

#' Generate a codelist from the intersection of different codelists. The generated
#' codelist will come out in alphabetical order.
#'
#' @inheritParams xDoc
#' @inheritParams keepOriginalDoc
#'
#' @return A codelist
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
#' # Intersect two codelists
#' codelist <- newCodelist(list("mood" = c(37110496L, 4226696L, 4304866L),
#'                              "manic" = c(37110496L, 4226696L)))
#'
#' intersectCodelists(codelist, keepOriginal = TRUE)
#'
#'
#' # Intersect two codelists_with_details
#' codelist <- asCodelistWithDetails(codelist, cdm)
#'
#' intersectCodelists(codelist, keepOriginal = FALSE)
#' }
intersectCodelists <- function(x,
                               keepOriginal = FALSE) {

  checkCodelist(x)
  omopgenerics::assertLogical(keepOriginal, length = 1)

  newName <- paste0("intersection_",paste0(names(x), collapse = "_"))

  if(inherits(x, "codelist")){
    newX <- list(Reduce(intersect, x))
    names(newX) <- newName

    if(isTRUE(keepOriginal)){
      newX <- purrr::list_flatten(list(x, newX)) |>
        omopgenerics::newCodelist()
    } else {
      newX <- newX |> omopgenerics::newCodelist()
    }
  }

  if(inherits(x, "codelist_with_details")){
    newX <- purrr::imap(
      x,
      ~ dplyr::mutate(.x, "codelist" = .y)) |>
      dplyr::bind_rows(.id = "codelist")

    newX <- newX |>
      dplyr::group_by(dplyr::across(-dplyr::all_of("codelist"))) |>
      dplyr::mutate("codelist" = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::filter(.data$codelist == length(names(x))) |>
      dplyr::select(-"codelist") |>
      dplyr::distinct()

    newX <- list("x" = newX)
    names(newX) <- newName

    if(isTRUE(keepOriginal)){
      newX <- purrr::list_flatten(list(x, newX)) |>
        omopgenerics::newCodelistWithDetails()
    }else{
      newX <- newX |> omopgenerics::newCodelistWithDetails()
    }
  }

  return(newX)
}
