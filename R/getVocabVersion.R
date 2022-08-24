

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
