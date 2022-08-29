

getVocabVersion <- function(db = NULL,
                            vocabularyDatabaseSchema=NULL,
                            arrowDirectory=NULL){

# link to vocab table
if(!is.null(db)){
if(!is.null(vocabularyDatabaseSchema)){
    vocabDb <-  dplyr::tbl(db, dplyr::sql(paste0(
      "SELECT * FROM ",
      vocabularyDatabaseSchema,
      ".vocabulary"
    )))
}
  if(is.null(vocabularyDatabaseSchema)){
    vocabDb <- dplyr::tbl(db, "vocabulary")
  }
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

getDomains <- function(db = NULL,
                       vocabularyDatabaseSchema=NULL,
                       arrowDirectory=NULL){

  # link to vocab table
  if(!is.null(db)){
    if(!is.null(vocabularyDatabaseSchema)){
      conceptDb <-  dplyr::tbl(db, dplyr::sql(paste0(
        "SELECT * FROM ",
        vocabularyDatabaseSchema,
        ".concept"
      )))
    }
    if(is.null(vocabularyDatabaseSchema)){
      conceptDb <- dplyr::tbl(db, "concept")
    }
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


getconceptClassId <- function(db = NULL,
                       vocabularyDatabaseSchema=NULL,
                       arrowDirectory=NULL,
                       domain = NULL){

  # link to vocab table
  if(!is.null(db)){
    if(!is.null(vocabularyDatabaseSchema)){
      conceptDb <-  dplyr::tbl(db, dplyr::sql(paste0(
        "SELECT * FROM ",
        vocabularyDatabaseSchema,
        ".concept"
      )))
    }
    if(is.null(vocabularyDatabaseSchema)){
      conceptDb <- dplyr::tbl(db, "concept")
    }
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
