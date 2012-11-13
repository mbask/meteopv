\name{getSunCoordinates}
\alias{getSunCoordinates}
\title{Estimates Sun azimuth and height above a location}
\usage{
  getSunCoordinates(locCoordinates, time)
}
\arguments{
  \item{time}{a POSIXct class object}

  \item{locCoordinates}{a vector holding longitude and
  latitude of a single location on earth}
}
\value{
  a matrix of azimuth and height
}
\description{
  Estimates Sun azimuth and height above a location
}
\author{
  Marco Bascietto \email{marco@bascietto.name}
}

