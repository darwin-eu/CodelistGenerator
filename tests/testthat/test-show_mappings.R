test_that("multiplication works", {
library(Eunomia)
library(DBI)
library(RSQLite)
untar(xzfile(system.file("sqlite", "cdm.tar.xz", package = "Eunomia"), open = "rb"),
       exdir =  tempdir())
db <- DBI::dbConnect(RSQLite::SQLite(), paste0(tempdir(),"\\cdm.sqlite"))
codes<-get_candidate_codes(keywords="a",
                   search.synonyms=TRUE,
                             fuzzy.match=TRUE,
                             exclude=NULL,
                             include.descendants=TRUE,
                             include.ancestor=FALSE,
                   db=db,
                   vocabulary_database_schema = "main")

mappings<-show_mappings(candidate_codelist= codes,
                 source_vocabularies=c("ATC"),
                 db=db,
                 vocabulary_database_schema = "main")
mappings2<-show_mappings(candidate_codelist= codes,
                source_vocabularies=c("ATC","ICD10CM","ICD10PCS" ,
                                              "ICD9CM",  "ICD9Proc" ,
                                              "LOINC","OPCS4","Read",
                                              "RxNorm" ,"RxNorm Extension",
                                              "SNOMED"),
                 db=db,
                 vocabulary_database_schema = "main")
expect_true(nrow(mappings2)>=nrow(mappings))
})
