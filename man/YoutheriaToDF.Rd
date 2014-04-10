\name{YoutheriaToDF}
\alias{YoutheriaToDF}
\title{Convert data returned from API to a dataframe}
\usage{
YoutheriaToDF(x)
}
\arguments{
  \item{x}{a list of trait data as returned by
  \code{fromJSON(getURL(url))}.}
}
\value{
A \code{data.frame} of melted trait data
}
\description{
Takes the list returned by \code{fromJSON(getURL(url))},
where url is the ValueByType controller in the YouTheria
API, and returns a data.frame. This code is faster than
using plyr.
}

