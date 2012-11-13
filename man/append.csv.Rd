\name{append.csv}
\alias{append.csv}
\title{Appends data to a CSV file or connection}
\usage{
  append.csv(x, file = "data/tempIrr.csv")
}
\arguments{
  \item{x}{a \code{data.frame} to be appended to an
  existing CSV file}

  \item{file}{CSV file name. Default value
  "\code{data/tempIrr.csv}"}
}
\description{
  \code{x} is appended to a csv file (if it already exists)
  or written to a new file. Calls internally
  \code{write.table}.
}
\author{
  Marco Bascietto \email{marco@bascietto.name}
}
\seealso{
  \code{\link{write.table}}
}

