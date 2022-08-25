
library(readr)
library(DBI)
library(RSQLite)
library(odbc)
library(here)
library(dplyr)
library(dbplyr)
library(stringr)
library(DT)
library(kableExtra)
devtools::load_all()


# usethis::edit_r_environ()
db<-DBI::dbConnect(RSQLite::SQLite(),
                   Sys.getenv("VocabSQlitePath"))
vocabularyDatabaseSchema<-"main"

# import local
library(arrow)
downloadVocab(db,
            vocabularyDatabaseSchema = vocabularyDatabaseSchema,
            dirOut=Sys.getenv("VocabArrowPath"),
            errorIfExists = FALSE,
            verbose = TRUE)
# using db
dementia_db<-getCandidateCodes(keywords="dementia",
                     domains="Condition",
                     db=db,
                     vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                     verbose=TRUE)
getMappings(dementia_db,
              nonStandardVocabularies="ICD10CM",
              db=db,
              vocabularyDatabaseSchema = vocabularyDatabaseSchema)
# using imported
dementia_arrow<-getCandidateCodes(keywords="dementia",
                    domains="Condition",
                     arrowDirectory = Sys.getenv("VocabArrowPath"),
                    verbose = TRUE)
getMappings(dementia_arrow,
              nonStandardVocabularies="ICD10CM",
              arrowDirectory = Sys.getenv("VocabArrowPath"))
