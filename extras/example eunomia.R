library(dplyr)
library(Eunomia)
library(stringr)
library(readr)
library(DBI)
devtools::load_all()

untar(xzfile(system.file("sqlite", "cdm.tar.xz", package = "Eunomia"), open = "rb"),
        exdir =  tempdir())
db <- dbConnect(RSQLite::SQLite(), paste0(tempdir(),"\\cdm.sqlite"))
vocabulary_database_schema<-"main"

get_candidate_codes(keywords="asthma",
                    db=db,
                    vocabulary_database_schema = "main")
get_candidate_codes(keywords="asthma",
                    search.synonyms=TRUE,
                              fuzzy.match=TRUE,
                              exclude=NULL,
                              include.descendants=TRUE,
                              include.ancestor=FALSE,
                    db=db,
                    vocabulary_schema = "main")
#
