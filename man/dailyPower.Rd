\name{dailyPower}
\alias{dailyPower}
\title{Integrates periodical power values}
\usage{
  dailyPower(dayData)
}
\arguments{
  \item{dayData}{a \code{data.frame} holding \code{time}
  and ay least 1 \code{pot} column}
}
\value{
  a summary \code{data.frame} o total power bound with
  columns from \code{daydata} whose name start with "year"
}
\description{
  Given a \code{data.frame} holding power values (W)
  measured of forecasted at date/time values in column
  \code{value}, the function integrates them into total
  values (kW). This is a function to be invoked on groups
  of hourly power data on a day level. Integration is
  performed by summing up area of trapezia formed by
  consecutive power values. Height of trapezia is given by
  the time lag between two power values, its bases are
  given by the two consecutive power values. #' # Calcola
  area del trapezio rettangolo formato da due successive
  misure di potenza e l'asse x
}
\author{
  Marco Bascietto \email{marco@bascietto.name}
}

