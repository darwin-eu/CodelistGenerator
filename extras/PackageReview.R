
# 1) Dependencies
# BiocManager::install("ComplexHeatmap")
library(pkgndep)
pkg <- pkgndep(here::here())
plot(pkg, fix_size = FALSE)
heaviness(pkg)



# 2) Speed
library(DBI)
library(here)
library(dplyr)
library(dbplyr)
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
vocabularyDatabaseSchema<-"omop21t2_cmbd"
getCandidateCodes(keywords="dementia",
                     domains="Condition",
                     db=db,
                     vocabularyDatabaseSchema =vocabularyDatabaseSchema,
                     verbose=TRUE)
# 52 seconds using sidiap PC



# 3) Memory usage
profvis::profvis({
getCandidateCodes(keywords="dementia",
                     domains="Condition",
                     db=db,
                     vocabularyDatabaseSchema =vocabularyDatabaseSchema,
                     verbose=TRUE)
})
