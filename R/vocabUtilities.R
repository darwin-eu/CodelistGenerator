

#' getVocabVersion
#'
#' @param cdm cdm_reference via CDMConnector
#'
#' @return
#' @export
#'
#' @examples
getVocabVersion <- function(cdm){

  error_message <- checkmate::makeAssertCollection()
  cdm_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdm_inherits_check,
                        add = error_message
  )
  if (!isTRUE(cdm_inherits_check)) {
    error_message$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::reportAssertions(collection = error_message)

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
#'
#' @return
#' @export
#'
#' @examples
getDomains <- function(cdm){

  error_message <- checkmate::makeAssertCollection()
  cdm_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdm_inherits_check,
                        add = error_message
  )
  if (!isTRUE(cdm_inherits_check)) {
    error_message$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::reportAssertions(collection = error_message)

  domains <-  cdm$concept %>%
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

  error_message <- checkmate::makeAssertCollection()
  cdm_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdm_inherits_check,
                        add = error_message
  )
  if (!isTRUE(cdm_inherits_check)) {
    error_message$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::reportAssertions(collection = error_message)

  vocabs <- cdm$concept %>%
    dplyr::select("vocabulary_id") %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull()

  return(vocabs)

}

#' getconceptClassId
#'
#' @param cdm cdm_reference via CDMConnector
#' @param domain Vocabulary domain
#'
#' @return
#' @export
#'
#' @examples
getconceptClassId <- function(cdm,
                       domain = NULL){

  error_message <- checkmate::makeAssertCollection()
  cdm_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdm_inherits_check,
                        add = error_message
  )
  if (!isTRUE(cdm_inherits_check)) {
    error_message$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::assert_character(domain,
                              add = error_message,
                              null.ok = TRUE
  )
  checkmate::reportAssertions(collection = error_message)

  # link to vocab table
  conceptDb <- cdm$concept

  if(!is.null(domain)){
  conceptDb <- conceptDb %>%
    dplyr::filter(.data$domain_id==.env$domain)
  }

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

  error_message <- checkmate::makeAssertCollection()
  cdm_inherits_check <- inherits(cdm, "cdm_reference")
  checkmate::assertTRUE(cdm_inherits_check,
                        add = error_message
  )
  if (!isTRUE(cdm_inherits_check)) {
    error_message$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  checkmate::assert_numeric(concept_id,
                              add = error_message
  )
  checkmate::reportAssertions(collection = error_message)

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
