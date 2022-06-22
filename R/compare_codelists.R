#' Compare two codelists
#'
#' @param codelist_1 Output of get_candidate_codes
#' @param codelist_2 Output of get_candidate_codes
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(CodelistGenerator)
#' db <- DBI::dbConnect(" Your database connection here " )
#' vocabulary_database_schema <- " Your vocabulary schema here "
#' asthma_codes <- get_candidate_codes(
#'   keywords = "asthma",
#'   db = db,
#'   vocabulary_database_schema = " Your vocabulary schema here ")
#' persistant_asthma_codes <- get_candidate_codes(
#'   keywords = "Persistent asthma",
#'   db = db,
#'   vocabulary_database_schema = " Your vocabulary schema here ")
#'   compare_codelists(
#'    codelist_1 = asthma_codes,
#'    codelist_2 = persistant_asthma_codes
#'  )

#' }
compare_codelists <- function(codelist_1,
                              codelist_2) {
  

All_list <- rbind(codelist_1, codelist_2) # row combine both list
duplicate_list <- All_list[duplicated(All_list), ] # create list duplicate
unique_list <- unique(All_list) # list of unique rows from both list

# function to return new column which indicate which list the concept came from. If return "Both" it means the concept contain in both list.
unique_list$check <-
  ifelse(is.na(
    match(
    
    paste0(unique_list$concept_id, unique_list$concept_name),
    
    paste0(duplicate_list$concept_id, duplicate_list$concept_name)
    
  )
  ),  ifelse(is.na(
    match(
      
      paste0(unique_list$concept_id, unique_list$concept_name),
      
      paste0(codelist_1$concept_id, codelist_1$concept_name)
      
    )
  ), deparse(substitute(codelist_2)), deparse(substitute(codelist_1))), "Both")

unique_list # return new list

}

