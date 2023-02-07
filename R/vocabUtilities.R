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
#' @return the vocabulary version being used
#' @export
#'
#' @examples
#' cdm <- mockVocabRef()
#' getVocabVersion(cdm = cdm)
#' DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
getVocabVersion <- function(cdm) {
  errorMessage <- checkmate::makeAssertCollection()
  cdmInheritsCheck <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdmInheritsCheck,
    add = errorMessage
  )
  if (!isTRUE(cdmInheritsCheck)) {
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
#' @return The domains of the cdm
#' @export
#'
#' @examples
#' cdm <- mockVocabRef()
#' getDomains(cdm = cdm)
#' DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
getDomains <- function(cdm,
                       standardConcept = "Standard") {
  errorMessage <- checkmate::makeAssertCollection()
  cdmInheritsCheck <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdmInheritsCheck,
    add = errorMessage
  )
  if (!isTRUE(cdmInheritsCheck)) {
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

  standardConcept <- tolower(standardConcept)
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

  domains <- conceptDb %>%
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
#' @return Names of available vocabularies
#' @export
#'
#' @examples
#' cdm <- mockVocabRef()
#' getVocabularies(cdm = cdm)
#' DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

getVocabularies <- function(cdm) {
  errorMessage <- checkmate::makeAssertCollection()
  cdmInheritsCheck <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdmInheritsCheck,
    add = errorMessage
  )
  if (!isTRUE(cdmInheritsCheck)) {
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
#' @return The concept class used for a given set of domains
#' @export
#'
#' @examples
#' cdm <- mockVocabRef()
#' getConceptClassId(cdm = cdm, domain = "drug")
#' DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

getConceptClassId <- function(cdm,
                              standardConcept = "Standard",
                              domain = NULL) {
  errorMessage <- checkmate::makeAssertCollection()
  cdmInheritsCheck <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdmInheritsCheck,
    add = errorMessage
  )
  if (!isTRUE(cdmInheritsCheck)) {
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

  if (!is.null(domain)) {
    conceptDb <- conceptDb %>%
      dplyr::filter(tolower(.data$domain_id) == tolower(.env$domain))
  }

  standardConcept <- tolower(standardConcept)
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

#' getDoseForm
#'
#' @param cdm cdm_reference via CDMConnector
#'
#' @return The dose forms available for drug concepts
#' @export
#'
#' @examples
#' cdm <- mockVocabRef()
#' getDoseForm(cdm = cdm)
#' DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

getDoseForm <- function(cdm) {
  errorMessage <- checkmate::makeAssertCollection()
  cdmInheritsCheck <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdmInheritsCheck,
    add = errorMessage
  )
  if (!isTRUE(cdmInheritsCheck)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::reportAssertions(collection = errorMessage)

  rxDoseForm <- cdm$concept_relationship %>%
    dplyr::filter(.data$relationship_id == "RxNorm has dose form") %>%
    dplyr::select("concept_id_2") %>%
    dplyr::rename("concept_id" = "concept_id_2") %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      cdm$concept %>%
        dplyr::select(
          "concept_id", "concept_name",
          "standard_concept"
        ),
      by = "concept_id"
    ) %>%
    dplyr::collect() %>%
    dplyr::pull("concept_name")

  return(rxDoseForm)
}

#' getDescendants
#'
#' @param cdm cdm_reference via CDMConnector
#' @param conceptId concpet_id to search
#'
#' @return The descendants of a given concept id
#' @export
#'
#' @examples
#' cdm <- mockVocabRef()
#' getDescendants(cdm = cdm, conceptId = 1)
#' DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
getDescendants <- function(cdm, conceptId) {
  errorMessage <- checkmate::makeAssertCollection()
  cdmInheritsCheck <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdmInheritsCheck,
    add = errorMessage
  )
  if (!isTRUE(cdmInheritsCheck)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::assert_numeric(conceptId,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  descendants <- cdm$concept_ancestor %>%
    dplyr::filter(.data$ancestor_concept_id %in% .env$conceptId) %>%
    dplyr::select("descendant_concept_id") %>%
    dplyr::distinct() %>%
    dplyr::rename("concept_id" = "descendant_concept_id") %>%
    dplyr::left_join(cdm$concept,
      by = "concept_id"
    ) %>%
    dplyr::collect()
  # return concept_id used along with descendants
  all <- dplyr::bind_rows(
    cdm$concept %>%
      dplyr::filter(.data$concept_id %in% .env$conceptId) %>%
      dplyr::collect(),
    descendants
  ) %>%
    dplyr::distinct() %>%
    dplyr::arrange("concept_id")

  return(all)
}
