% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drugCodes.R
\name{getDrugIngredientCodes}
\alias{getDrugIngredientCodes}
\title{Get descendant codes for drug ingredients}
\usage{
getDrugIngredientCodes(
  cdm,
  name = NULL,
  doseForm = NULL,
  doseUnit = NULL,
  routeCategory = NULL,
  ingredientRange = c(1, Inf),
  type = "codelist"
)
}
\arguments{
\item{cdm}{cdm_reference via CDMConnector}

\item{name}{Names of ingredients of interest. For example, c("acetaminophen",
"codeine"), would result in a list of length two with the descendant
concepts for these two particular drug ingredients.}

\item{doseForm}{Only descendants codes with the specified dose form
will be returned. If NULL, descendant codes will be returned regardless
of dose form.}

\item{doseUnit}{Only descendants codes with the specified dose unit
will be returned. If NULL, descendant codes will be returned regardless
of dose unit}

\item{routeCategory}{Only descendants codes with the specified route
will be returned. If NULL, descendant codes will be returned regardless
of route category.}

\item{ingredientRange}{Used to restrict descendant codes to those
associated with a specific number of ingredients. Must be a vector of length
two with the first element the minimum number of ingredients allowed and
the second the maximum. A value of c(2, 2) would restrict to only concepts
associated with two ingredients.}

\item{type}{Can be "codelist", "codelist_with_details", or
"concept_set_expression"}
}
\value{
Concepts with their format based on the type argument.
}
\description{
Get descendant codes for drug ingredients
}
\examples{
\dontrun{
cdm <- mockVocabRef()
getDrugIngredientCodes(cdm = cdm, name = "Adalimumab")
CDMConnector::cdmDisconnect(cdm)
}
}
