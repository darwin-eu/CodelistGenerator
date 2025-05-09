% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/documentation_helper.R
\name{headerStrataDoc}
\alias{headerStrataDoc}
\title{Helper for consistent documentation of \code{header}.}
\arguments{
\item{header}{A vector specifying the elements to include in the header. The
order of elements matters, with the first being the topmost header.
The header vector can contain one of the following variables: "cdm_name",
"codelist_name", "standard_concept_name", "standard_concept_id",
"estimate_name", "source_concept_name", "source_concept_id", "domain_id". If
results are stratified, "year", "sex", "age_group" can also be used.
Alternatively, it can include other names to use as overall header labels.}
}
\description{
Helper for consistent documentation of \code{header}.
}
\keyword{internal}
