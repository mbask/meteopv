\name{scrapeMeteo}
\alias{scrapeMeteo}
\title{Scrape meteorological data from \url{meteo.enel.it}

\code{enel.it} does not provide any API. Meteorological parameters, including temperature and irradiance, are scraped from its website. Therefore even sligh changes to \code{HTML} code may break the code.}
\usage{
  scrapeMeteo(place,
    webAddress = "http://meteo.enel.it/dettagli/",
    timeOfDayNum = 8, variableNum = 12)
}
\arguments{
  \item{place}{integer code linked to a geographical
  location in Enel DB. Check
  \url{meteo.enel.it}{meteo.enel.it} to get the code for
  your town or city}

  \item{webAddress}{the \code{http} address where
  meteorological data is to be scraped from. Defaults to
  \url{http://meteo.enel.it/dettagli/}}

  \item{timeOfDayNum}{how many times a day forecasts are
  provided. Defaults to 8, \emph{i.e.} every 3 hours.}

  \item{variableNum}{how many variables each forects
  provide. Defaults to 12.}
}
\value{
  a \code{data.frame} of 3 columns: \code{time},
  \code{variable}, \code{value}
}
\description{
  Scrape meteorological data from \url{meteo.enel.it}

  \code{enel.it} does not provide any API. Meteorological
  parameters, including temperature and irradiance, are
  scraped from its website. Therefore even sligh changes to
  \code{HTML} code may break the code.
}
\author{
  Marco Bascietto \email{marco@bascietto.name}
}

