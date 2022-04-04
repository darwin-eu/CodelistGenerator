


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
