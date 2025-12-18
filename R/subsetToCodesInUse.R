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

#' Filter a codelist to keep only the codes being used in patient records
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams minimumCountDoc
#' @inheritParams tableDoc
#'
#' @return The filtered codelist with only the codes used in the database
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omopgenerics)
#' cdm <- mockVocabRef("database")
#' codes <- getCandidateCodes(cdm = cdm,
#'                            keywords = "arthritis",
#'                            domains = "Condition",
#'                            includeDescendants = FALSE)
#' x <- subsetToCodesInUse(newCodelist(list("cs1" = codes$concept_id,
#'                                "cs2" = 999)),
#'                                 cdm = cdm)
#'
#' x
#' CDMConnector::cdmDisconnect(cdm)
#' }
subsetToCodesInUse <- function(x,
                               cdm,
                               minimumCount = 0L,
                               table = c("condition_occurrence",
                                         "device_exposure",
                                         "drug_exposure",
                                         "measurement",
                                         "observation",
                                         "procedure_occurrence",
                                         "visit_occurrence")){

  # Initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm,
                                           requiredTables = c("achilles_analysis",
                                                              "achilles_results",
                                                              "achilles_results_dist"))
  omopgenerics::assertNumeric(minimumCount, integerish = TRUE, min = 0, length = 1)
  omopgenerics::assertChoice(table, choices = c("condition_occurrence",
                                                "device_exposure",
                                                "drug_exposure",
                                                "measurement",
                                                "observation",
                                                "procedure_occurrence",
                                                "visit_occurrence"))
  checkCodelist(x, allowConceptSetExpression = FALSE)

  x_original <- x
  if(inherits(x_original, "codelist_with_details")){
    x <- asCodelist(x)
    newX <- omopgenerics::emptyCodelistWithDetails()
  }
  if(inherits(x_original, "codelist")){
    newX <- omopgenerics::emptyCodelist()
  }

  dbCodes <- codesInUse(cdm = cdm,
                        minimumCount = minimumCount,
                        table = table)

  if(is.null(dbCodes)){
    for(i in seq_along(x)){
      cli::cli_inform("No codes from any codelist found in the database")
      return(invisible(omopgenerics::emptyCodelist()))
    }
  } else {
    for(i in seq_along(x)){

      x[[i]] <- intersect(x[[i]], dbCodes)

      if(!length(x[[i]]) >= 1){
        cli::cli_inform("No codes from codelist {names(x)[i]} found in the database")
        x_original[[i]] <- x[[i]]
      }

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
  }

  newX <- dropEmptyCodelist(x_original, newX)

  return(newX)
}


codesInUse <- function(cdm,
                       minimumCount = 0L,
                       table = c("condition_occurrence",
                                 "device_exposure",
                                 "drug_exposure",
                                 "measurement",
                                 "observation",
                                 "procedure_occurrence",
                                 "visit_occurrence")){

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm,
                                           requiredTables = c("achilles_analysis",
                                                              "achilles_results",
                                                              "achilles_results_dist"))
  omopgenerics::assertNumeric(minimumCount, integerish = TRUE, min = 0, length = 1)
  omopgenerics::assertCharacter(table)

  codes <- fetchAchillesCodesInUse(cdm, minimumCount = minimumCount)

  codes
}
