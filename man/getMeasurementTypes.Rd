\name{getMeasurementTypes}
\alias{getMeasurementTypes}
\title{Get Measurement Types}
\usage{
getMeasurementTypes(measurementType = NULL)
}
\arguments{
  \item{measurementType}{If \code{NULL} (default), then all
  measurement types are returned. Can also be 'numeric' or
  'character' (or a list of either type) and will filter by
  Id and Name respectivly in the resulting data.frame.}
}
\value{
A dataframe of measurement types giving their Id and Name
}
\description{
Retrieves a \code{data.frame} of measurement types
available from Youtheria.
}
\examples{
\dontrun{

getMeasurementTypes()
getMeasurementTypes('Body Mass')
getMeasurementTypes(c('Body Mass','Limb Length'))
getMeasurementTypes(1)
getMeasurementTypes(1:3)
}
}

