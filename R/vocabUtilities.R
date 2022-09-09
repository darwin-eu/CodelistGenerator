

getVocabVersion <- function(cdm){

# link to vocab table
vocabDb <- cdm$vocabulary

# get overall version
version <- vocabDb %>%
    dplyr::rename_with(tolower) %>%
    dplyr::filter(.data$vocabulary_id == "None") %>%
    dplyr::select("vocabulary_version") %>%
    dplyr::collect() %>%
    dplyr::pull()

return(version)

}

getDomains <- function(cdm){

  # link to vocab table
    conceptDb <- cdm$concept

  domains <- conceptDb %>%
    dplyr::select("domain_id") %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull()

  return(domains)

}

getVocabularies <- function(cdm){

  # link to vocab table
      conceptDb <- cdm$concept

  vocabs <- conceptDb %>%
    dplyr::select("vocabulary_id") %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull()

  return(vocabs)

}

getconceptClassId <- function(cdm=NULL,
                       arrowDirectory=NULL,
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
