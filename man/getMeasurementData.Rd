\name{getMeasurementData}
\alias{getMeasurementData}
\title{Get a table of trait measurements from YouTheria}
\usage{
getMeasurementData(measurementType = NULL, MSW93Binomial = NULL,
  MSW05Binomial = NULL, country = NULL, StudyUnitId = NULL,
  locationData = FALSE, locationOnly = TRUE)
}
\arguments{
  \item{measurementType}{Measurement types to collect data
  for. If \code{NULL} (default), all measurement types are
  returned. Can also be 'numeric' or 'character' (or a list
  of either type) and will filter by MeasurementTypeID and
  the measurement respectivly. MeasurementTypeIDs and names
  can be found using getMeasurementTypes().}

  \item{MSW93Binomial}{Character giving the latin name of a
  species (or list of species) for which measurements are
  required. Naming should follow Mammal Species of the
  World 1993.}

  \item{MSW05Binomial}{Character giving the latin name of a
  species (or list of species) for which measurements are
  required. Naming should follow Mammal Species of the
  World 2005.}

  \item{country}{Character specifying the country from
  which you wish to collect data.  If \code{NULL} all data
  is retrieved}

  \item{StudyUnitId}{Numeric specifying the StudyUnitId
  from which you wish to collect data.  If \code{NULL} all
  data is retrieved}

  \item{locationData}{Logial dictating whether location
  information should be added to the output. Defualt is
  \code{FALSE} but is set to \code{TRUE} if either
  StudyUnitId or country are specified.}

  \item{locationOnly}{Logical only used if locationData is
  TRUE. If \code{TRUE} data is only be returned if it has
  location information.}
}
\value{
A \code{data.frame} with each row giving a trait
measurement
}
\description{
Retrieves a \code{data.frame} of trait measurements with
facilities to select by location, species name and/or
measurement type.
}
\examples{
\dontrun{

#Select measurement type by id
getMeasurementData(1)
getMeasurementData(c(1:3))

#Select measurement type by name
getMeasurementData('Body Mass')
getMeasurementData(c('Body Mass','Population Density'))

#Select by measurement type and species name
getMeasurementData(measurementType = 1,
                   MSW93Binomial=c('Pongo pygmaeus','Peroryctes raffrayana'))


}
}

