# Copyright 2022 DARWIN EU®
#
# This file is part of IncidencePrevalence
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

#' getVocabVersion
#'
#' @param cdm cdm_reference via CDMConnector
#'
#' @return
#' @export
#'
#' @examples
getVocabVersion <- function(cdm){

  errorMessage <- checkmate::makeAssertCollection()
  cdm_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdm_inherits_check,
                        add = errorMessage
  )
  if (!isTRUE(cdm_inherits_check)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::reportAssertions(collection = errorMessage)

  version <- cdm$vocabulary %>%
    dplyr::rename_with(tolower) %>%
    dplyr::filter(.data$vocabulary_id == "None") %>%
    dplyr::select("vocabulary_version") %>%
    dplyr::collect() %>%
    dplyr::pull()

return(version)

}

#' getDomains
#'
#' @param cdm cdm_reference via CDMConnector
#' @param standardConcept  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#'
#' @return
#' @export
#'
#' @examples
getDomains <- function(cdm,
                       standardConcept = "Standard"){

  errorMessage <- checkmate::makeAssertCollection()
  cdm_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdm_inherits_check,
                        add = errorMessage
  )
  if (!isTRUE(cdm_inherits_check)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::assertVector(standardConcept, add = errorMessage)
  standardConceptCheck <- all(tolower(standardConcept) %in%
                                c(
                                  "standard",
                                  "classification",
                                  "non-standard"
                                ))
  checkmate::assertTRUE(standardConceptCheck,
                        add = errorMessage
  )
  if (!isTRUE(standardConceptCheck)) {
    errorMessage$push(
      "- standardConcept should be one or more of Standard, Non-stanadard, or Classification"
    )
  }
  checkmate::reportAssertions(collection = errorMessage)

  conceptDb <- cdm$concept

  standardConcept<-tolower(standardConcept)
  conceptDb <- conceptDb %>%
    dplyr::mutate(
      standard_concept = ifelse(is.na(.data$standard_concept),
                                "non-standard", .data$standard_concept
      )
    ) %>%
    dplyr::mutate(
      standard_concept = ifelse(.data$standard_concept == "C",
                                "classification", .data$standard_concept
      )
    ) %>%
    dplyr::mutate(
      standard_concept = ifelse(.data$standard_concept == "S",
                                "standard", .data$standard_concept
      )
    ) %>%
    dplyr::filter(.data$standard_concept %in% .env$standardConcept)

  domains <-  conceptDb %>%
    dplyr::select("domain_id") %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull()

  return(domains)

}

#' getVocabularies
#'
#' @param cdm cdm_reference via CDMConnector
#'
#' @return
#' @export
#'
#' @examples
getVocabularies <- function(cdm){

  errorMessage <- checkmate::makeAssertCollection()
  cdm_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdm_inherits_check,
                        add = errorMessage
  )
  if (!isTRUE(cdm_inherits_check)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::reportAssertions(collection = errorMessage)

  vocabs <- cdm$concept %>%
    dplyr::select("vocabulary_id") %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull()

  return(vocabs)

}

#' getConceptClassId
#'
#' @param cdm cdm_reference via CDMConnector
#' @param standardConcept  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param domain Vocabulary domain
#'
#' @return
#' @export
#'
#' @examples
getConceptClassId <- function(cdm,
                              standardConcept = "Standard",
                       domain = NULL){

  errorMessage <- checkmate::makeAssertCollection()
  cdm_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdm_inherits_check,
                        add = errorMessage
  )
  if (!isTRUE(cdm_inherits_check)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::assertVector(standardConcept, add = errorMessage)
  standardConceptCheck <- all(tolower(standardConcept) %in%
                                c(
                                  "standard",
                                  "classification",
                                  "non-standard"
                                ))
  if (!isTRUE(standardConceptCheck)) {
    errorMessage$push(
      "- standardConcept should be one or more of Standard, Non-stanadard, or Classification"
    )
  }
  checkmate::assert_character(domain,
                              add = errorMessage,
                              null.ok = TRUE
  )
  checkmate::reportAssertions(collection = errorMessage)

  # link to vocab table
  conceptDb <- cdm$concept

  if(!is.null(domain)){
  conceptDb <- conceptDb %>%
    dplyr::filter(.data$domain_id==.env$domain)
  }

  standardConcept<-tolower(standardConcept)
  conceptDb <- conceptDb %>%
    dplyr::mutate(
      standard_concept = ifelse(is.na(.data$standard_concept),
                                "non-standard", .data$standard_concept
      )
    ) %>%
    dplyr::mutate(
      standard_concept = ifelse(.data$standard_concept == "C",
                                "classification", .data$standard_concept
      )
    ) %>%
    dplyr::mutate(
      standard_concept = ifelse(.data$standard_concept == "S",
                                "standard", .data$standard_concept
      )
    ) %>%
    dplyr::filter(.data$standard_concept %in% .env$standardConcept)

  # get overall version
  conceptClassId <- conceptDb %>%
    dplyr::select("concept_class_id") %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull()

  return(conceptClassId)

}

#' getDescendants
#'
#' @param cdm cdm_reference via CDMConnector
#' @param concept_id concpet_id to search
#'
#' @return
#' @export
#'
#' @examples
getDescendants <- function(cdm, concept_id){

  errorMessage <- checkmate::makeAssertCollection()
  cdm_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdm_inherits_check,
                        add = errorMessage
  )
  if (!isTRUE(cdm_inherits_check)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::assert_numeric(concept_id,
                              add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

descendants<- cdm$concept_ancestor %>%
    dplyr::filter(.data$ancestor_concept_id %in%  .env$concept_id) %>%
    dplyr::select("descendant_concept_id") %>%
    dplyr::distinct() %>%
    dplyr::rename("concept_id" = "descendant_concept_id") %>%
    dplyr::left_join(cdm$concept,
              by = "concept_id") %>%
    dplyr::collect()
# return concept_id used along with descendants
all<-dplyr::bind_rows(cdm$concept %>%
  dplyr::filter(.data$concept_id %in% .env$concept_id) %>%
  dplyr::collect(),
  descendants) %>%
  dplyr::distinct() %>%
  dplyr::arrange(concept_id)

  return(all)
}
