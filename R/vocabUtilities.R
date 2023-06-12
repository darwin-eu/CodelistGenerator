# Copyright 2023 DARWIN EU®
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
#' @param withAncestor If TRUE, return column with ancestor. In case of multiple
#' ancestors, concepts will be separated by ";"
#' @param doseForm Only descendants codes with the specified drug dose form
#' will be returned. If NULL, descendant codes will be returned regardless
#' of dose form.
#'
#' @return The descendants of a given concept id
#' @export
#'
#' @examples
#' cdm <- mockVocabRef()
#' getDescendants(cdm = cdm, conceptId = 1)
#' DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

getDescendants <- function(cdm,
                           conceptId,
                           withAncestor = FALSE,
                           doseForm = NULL) {

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

if(isFALSE(withAncestor)){
  descendants <- getDescendantsOnly(cdm, conceptId, doseForm)}

  if(isTRUE(withAncestor)){
    descendants <- getDescendantsAndAncestor(cdm, conceptId, doseForm)}

  return(descendants)
}

getDescendantsOnly <- function(cdm, conceptId, doseForm) {
  descendants <- cdm$concept_ancestor %>%
    dplyr::filter(.data$ancestor_concept_id %in% .env$conceptId) %>%
    dplyr::select("descendant_concept_id") %>%
    dplyr::distinct() %>%
    dplyr::rename("concept_id" = "descendant_concept_id") %>%
    dplyr::left_join(cdm$concept,
                     by = "concept_id")

  if(!is.null(doseForm)){
    descendantDoseForms <- getPresentDoseForms(cdm, concepts = descendants)
  }

  descendants <- descendants  %>%
    dplyr::collect()

  if(!is.null(doseForm)){
    descendants <-  filterOnDoseForm(concepts = descendants,
                       conceptDoseForms = descendantDoseForms,
                     doseForm = doseForm)
  }

  # nb conceptId will also be a descendant of itself (if we don't specify dose)
  return(descendants)
}

getDescendantsAndAncestor <- function(cdm, conceptId, doseForm) {

  descendants <- cdm$concept_ancestor %>%
    dplyr::inner_join(dplyr::tibble(ancestor_concept_id = as.integer(conceptId)),
                      by = "ancestor_concept_id",
                      copy = TRUE) %>%
    dplyr::rename("concept_id" = "descendant_concept_id") %>%
    dplyr::left_join(cdm$concept,
                     by = "concept_id") %>%
    dplyr::mutate(name = paste0("concept_", .data$ancestor_concept_id))

  if(!is.null(doseForm)){
    descendantDoseForms <- getPresentDoseForms(cdm, concepts = descendants)
  }

  descendants <- descendants %>%
    dplyr::collect()

    if(nrow(descendants)>0){
descendants <- descendants %>%
        tidyr::pivot_wider(names_from = "name",
                           values_from = "ancestor_concept_id")

  # one row per concept, with ancestor (of which there may be multiple)
  working_cols <- stringr::str_subset(string = colnames(descendants),
                                      pattern = paste(c(colnames(cdm$concept),
                                                        colnames(cdm$concept_ancestor)),
                                                      collapse = "|"),
                                      negate = TRUE)

descendants <- descendants %>%
     tidyr::unite(col="ancestor_concept_id",
                  working_cols, sep=";")
# quicker to replace NAs afterwards rather than inside unite
# (especially when there are many columns)
descendants$ancestor_concept_id <- stringr::str_replace_all(
  string = descendants$ancestor_concept_id,
  pattern = ";NA|NA;",
  replacement = ""
)
    }

  if(!is.null(doseForm)){
    descendants <-  filterOnDoseForm(concepts = descendants,
                                     conceptDoseForms = descendantDoseForms,
                                     doseForm = doseForm)
  }

  # nb conceptId will also be a descendant of itself
  return(descendants)

}

getPresentDoseForms <- function(cdm, concepts){

  presentDoseForms <- concepts %>%
    dplyr::left_join(
      cdm$concept_relationship %>%
        dplyr::filter(.data$relationship_id == "RxNorm has dose form") %>%
        dplyr::select("concept_id_1", "concept_id_2") %>%
        dplyr::rename("concept_id" = "concept_id_2") %>%
        dplyr::distinct() %>%
        dplyr::left_join(cdm$concept, by = "concept_id") %>%
        dplyr::select("concept_id_1", "concept_name") %>%
        dplyr::rename("concept_id"="concept_id_1",
                      "dose_form"="concept_name")  ,
      by ="concept_id"
    ) %>%
    dplyr::select("concept_id", "dose_form") %>%
    dplyr::collect()

  presentDoseForms <- presentDoseForms %>%
    dplyr::group_by(.data$concept_id) %>%
    dplyr::mutate(seq = dplyr::row_number()) %>%
    tidyr::pivot_wider(
      names_from = "seq",
      values_from = "dose_form"
    )
  presentDoseForms <- presentDoseForms %>%
    tidyr::unite(
      col = "dose_form", 2:ncol(presentDoseForms), sep = "; ",
      na.rm = TRUE
    )
  return(presentDoseForms)

}

filterOnDoseForm <- function(concepts, conceptDoseForms, doseForm){
  concepts <- concepts %>%
    dplyr::inner_join(
      conceptDoseForms %>%
        dplyr::filter(stringr::str_detect(
          string = tolower(.data$dose_form),
          pattern = paste(tolower(.env$doseForm),
                          collapse = "|"
          )
        )) %>%
        dplyr::select("concept_id"),
      by = "concept_id")

  return(concepts)

}
