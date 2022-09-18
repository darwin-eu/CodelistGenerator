

#' getVocabVersion
#'
#' @param cdm cdm_reference via CDMConnector
#'
#' @return
#' @export
#'
#' @examples
getVocabVersion <- function(cdm){

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
getconceptClassId <- function(cdm=NULL,
                       domain = NULL){

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
