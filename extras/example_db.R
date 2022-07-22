
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

db <-DBI::dbConnect(odbc::odbc(),
                      Driver   = "ODBC Driver 11 for SQL Server",
                      Server   = Sys.getenv("darwinDbDatabaseServer"),
                      Database = Sys.getenv("darwinDbDatabase"),
                      UID      = Sys.getenv("darwinDbUser"),
                      PWD      = Sys.getenv("darwinDbPassword"),
                      Port     = Sys.getenv("darwinDbDatabasePort"))
vocabularyDatabaseSchema<-Sys.getenv("darwinDbCdmSchema")

dementia_codes<-getCandidateCodes(keywords="dementia",
                                    searchSource = TRUE,
                     domains="Condition",
                     db=db,
                     vocabularyDatabaseSchema =vocabularyDatabaseSchema,
                     verbose=TRUE)
show_mappings(dementia_codes,
              source_vocabularies="ICD10CM",
              db=db,
              vocabulary_database_schema =vocabulary_database_schema)
profvis::profvis({
colonoscopy_codes<-get_candidate_codes(keywords="colonoscopy",
                     domains="Procedure",
                     include_descendants=TRUE,
                     include_ancestor=TRUE,
                     db=db,
                     vocabulary_database_schema =vocabulary_database_schema )
})

# metformin_codes<-get_candidate_codes(keywords="metformin",
#                      domains="Drug",
#                      db=db,
#                      vocabulary_database_schema =vocabulary_database_schema,
#                      verbose = TRUE)


## test Adam´s DatabaseConnector
# devtools::install_github("ablack3/DatabaseConnector", ref="dbplyr")
# library(DatabaseConnector)
# server<-Sys.getenv("SERVER_FEB22")
# connectionDetails <-DatabaseConnector::downloadJdbcDrivers("postgresql", here::here())
# connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
#                                                                 server =server,
#                                                                 user = user,
#                                                                 password = password,
#                                                                 port = port ,
#                                                                 pathToDriver = here::here())
# db<-connect(connectionDetails)
# dementia_codes<-get_candidate_codes(keywords="dementia",
#                      domains="Condition",
#                      db=db,
#                      vocabulary_database_schema =vocabulary_database_schema )
