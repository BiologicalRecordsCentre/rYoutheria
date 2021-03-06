% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getLocData.R
\name{getLocData}
\alias{getLocData}
\title{Get location information from YouTheria}
\usage{
getLocData(country = NULL, StudyUnitId = NULL)
}
\arguments{
\item{country}{Character specifying the country within which to search for locations.
for a list of countries used getCountries().}

\item{StudyUnitId}{Numeric specifying the StudyUnitId to search for}
}
\value{
A dataframe in which each rows gives the details of a study unit
}
\description{
Retrieves location information stored on YouTheria
}
\examples{
\dontrun{
# Get a dataframe of Indian study units
Indian_StudyUnits <- getLocData(country = 'India')
}

}

