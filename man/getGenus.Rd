\name{getGenus}
\alias{getGenus}
\title{Get a table of genera}
\usage{
getGenus(genus = NULL)
}
\arguments{
  \item{genus}{If \code{NULL} (default), then a lis of all
  genera are returned. Can also be 'numeric' or 'character'
  (or a list of either type) and will filter by Id and Name
  respectivly in the resulting data.frame.}
}
\value{
A dataframe of genera giving their Id and Name
}
\description{
Retrieves a \code{data.frame} of generas available from
Youtheria.
}
\examples{
\dontrun{

getGenus()
getGenus('Antilocapra')
getGenus(c('Antilocapra','Damaliscus'))
getGenus(1)
getGenus(1:5)
}
}

