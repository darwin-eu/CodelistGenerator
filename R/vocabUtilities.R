

getVocabVersion <- function(cdm=NULL,
                            arrowDirectory=NULL){

# link to vocab table
if(!is.null(cdm)){
    vocabDb <- cdm$vocabulary
}

if(!is.null(arrowDirectory)){
vocabDb <-  arrow::read_parquet(paste0(arrowDirectory,
                           "/vocabulary.parquet"),
                    as_data_frame = FALSE)
}

# get overall version
version <- vocabDb %>%
    dplyr::rename_with(tolower) %>%
    dplyr::filter(.data$vocabulary_id == "None") %>%
    dplyr::select("vocabulary_version") %>%
    dplyr::collect() %>%
    dplyr::pull()

return(version)

}

getDomains <- function(cdm=NULL,
                       arrowDirectory=NULL){

  # link to vocab table
  if(!is.null(cdm)){
      conceptDb <- cdm$concept
  }

  if(!is.null(arrowDirectory)){
    conceptDb <-  arrow::read_parquet(paste0(arrowDirectory,
                                           "/concept.parquet"),
                                    as_data_frame = FALSE)
  }

  domains <- conceptDb %>%
    dplyr::select("domain_id") %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull()

  return(domains)

}

getVocabularies <- function(cdm=NULL,
                            arrowDirectory=NULL){

  # link to vocab table
  if(!is.null(cdm)){
      conceptDb <- cdm$concept
  }

  if(!is.null(arrowDirectory)){
    conceptDb <-  arrow::read_parquet(paste0(arrowDirectory,
                                             "/concept.parquet"),
                                      as_data_frame = FALSE)
  }

  vocabs <- conceptDb %>%
    dplyr::select("vocab_id") %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull()

  return(vocabs)

}

getconceptClassId <- function(cdm=NULL,
                       arrowDirectory=NULL,
                       domain = NULL){

  # link to vocab table
  if(!is.null(cdm)){
      conceptDb <- cdm$concept
  }

  if(!is.null(arrowDirectory)){
    conceptDb <-  arrow::read_parquet(paste0(arrowDirectory,
                                             "/concept.parquet"),
                                      as_data_frame = FALSE)
  }

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
