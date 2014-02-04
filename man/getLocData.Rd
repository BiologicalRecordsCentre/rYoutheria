\name{getLocData}
\alias{getLocData}
\title{Get location information from YouTheria}
\usage{
getLocData(country = NULL, StudyUnitId = NULL)
}
\arguments{
  \item{country}{Character specifying the country within
  which to search for locations. for a list of countries
  used getCountries().}

  \item{StudyUnitId}{Numeric specifying the StudyUnitId to
  search for}
}
\value{
A dataframe in which each rows gives the details of a study
unit
}
\description{
Retrieves location information store on YouTheria
}
\examples{
\dontrun{

getLocData(country = 'India')

}
}

