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
#' @inheritParams domainDoc
#' @param negate If FALSE, only concepts with the domain specified will be
#' returned. If TRUE, concepts with the domain specified will be excluded.
#'
#' @return The codelist with only those concepts associated with the domain
#' (if negate = FALSE) or the codelist without those concepts associated with
#' the domain (if negate = TRUE).
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' cdm <- mockVocabRef()
#' codes <- subsetOnDomain(
#'               x = list("codes" = c(10,13,15)),
#'               cdm = cdm,
#'               domain = "Drug")
#' codes
#' }
subsetOnDomain <- function(x,
                           cdm,
                           domain,
                           negate = FALSE){

  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::assertCharacter(domain)
  omopgenerics::assertLogical(negate)
  x <- omopgenerics::newCodelist(x)

  tableCodelist <- paste0(omopgenerics::uniqueTableName(),
                          omopgenerics::uniqueId())

  for(i in seq_along(x)){
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = tableCodelist,
                                     table = dplyr::tibble(concept_id = x[[i]]),
                                     overwrite = TRUE,
                                     temporary = FALSE)
    x[[i]] <- cdm[[tableCodelist]] |>
      dplyr::inner_join(cdm[["concept"]] ,
                        by = "concept_id") |>
      dplyr::select("concept_id",
                    "domain_id") |>
      dplyr::distinct() |>
      dplyr::collect()

    if(isTRUE(negate)){
      x[[i]] <- x[[i]] |>
        dplyr::filter(!tolower(.data$domain_id) %in% tolower(.env$domain))
    }else{
      x[[i]] <- x[[i]] |>
        dplyr::filter(tolower(.data$domain_id) %in% tolower(.env$domain))
    }

    x[[i]] <- x[[i]] |>
      dplyr::pull("concept_id")

    x[[i]] <- sort(unique(x[[i]]))
  }

  x <- x |>
    vctrs::list_drop_empty()

  if(length(x) == 0){
    x <- omopgenerics::emptyCodelist()
  }

  CDMConnector::dropTable(cdm = cdm, name = tableCodelist)

  return(x)

}
