
library(readr)
library(DBI)
library(RSQLite)
library(here)
library(dplyr)
library(stringr)
library(DT)
library(kableExtra)
devtools::load_all()


# usethis::edit_r_environ()
server_dbi<-Sys.getenv("SERVER_DBI_FEB22")
user<-Sys.getenv("DB_USER_FEB22")
password<- Sys.getenv("DB_PASSWORD_FEB22")
port<-Sys.getenv("DB_PORT_FEB22")
host<-Sys.getenv("DB_HOST_FEB22")

db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password)
vocabulary_database_schema<-"omop21t2_cmbd"
# tbl(db, sql(paste0("SELECT * FROM ",
#                                         vocabulary_database_schema,
#                                         ".concept")))

dementia_codes<-get_candidate_codes(keywords="dementia",
                     domains="Condition",
                     db=db,
                     vocabulary_database_schema =vocabulary_database_schema )
show_mappings(dementia_codes,
              source_vocabularies="ICD10CM",
              db=db,
              vocabulary_database_schema =vocabulary_database_schema)

colonoscopy_codes<-get_candidate_codes(keywords="colonoscopy",
                     domains="Procedure",
                     include_descendants=TRUE,
                     include_ancestor=TRUE,
                     db=db,
                     vocabulary_database_schema =vocabulary_database_schema )

metformin_codes<-get_candidate_codes(keywords="metformin",
                     domains="Drug",
                     db=db,
                     vocabulary_database_schema =vocabulary_database_schema,
                     verbose = TRUE)


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
