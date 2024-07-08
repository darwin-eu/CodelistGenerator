% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codesInUse.R
\name{codesInUse}
\alias{codesInUse}
\title{Use achilles counts to get codes used in the database}
\usage{
codesInUse(
  cdm,
  minimumCount = 0,
  table = c("condition_occurrence", "device_exposure", "drug_exposure", "measurement",
    "observation", "procedure_occurrence", "visit_occurrence")
)
}
\arguments{
\item{cdm}{cdm_reference via CDMConnector}

\item{minimumCount}{Any codes with a frequency under this will be removed.}

\item{table}{cdm table}
}
\value{
A list of integers indicating codes being used in the database.
}
\description{
Use achilles counts to get codes used in the database
}
\examples{
\dontrun{
cdm <- mockVocabRef("database")
x <- codesInUse(cdm = cdm)
x
CDMConnector::cdmDisconnect(cdm)
}
}