\name{append.csv}
\alias{append.csv}
\title{Appends data to a CSV file or connection

Calls internally \code{write.table}.}
\usage{
  append.csv(x, file = "data/tempIrr.csv")
}
\arguments{
  \item{x}{a \code{data.frame} to be appended to an
  existing CSV file}

  \item{file}{CSV file name. Defaults to
  \code{data/tempIrr.csv}}
}
\value{
  a \code{data.frame} of 3 columns: \code{time},
  \code{variable}, \code{value}
}
\description{
  Appends data to a CSV file or connection

  Calls internally \code{write.table}.
}
\author{
  Marco Bascietto \email{marco@bascietto.name}
}
\seealso{
  \code{\link{write.table}}
}

