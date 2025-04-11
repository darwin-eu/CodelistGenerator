codelistFromCodelistWithDetails <- function(x){

  if(isFALSE(inherits(x, "codelist_with_details"))){
    cli::cli_abort("x is not class codelist_with_details but {class(x)}")
  }

  for(i in seq_along(x)){
    x[[i]] <- x[[i]] |>
      dplyr::pull("concept_id")
  }

  x <- omopgenerics::newCodelist(x)

  x

}
