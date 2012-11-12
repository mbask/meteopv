\name{scrapeMeteo}
\alias{scrapeMeteo}
\title{Scrape meteorological data from \url{meteo.enel.it}}
\usage{
  scrapeMeteo(location,
    webAddress = "http://meteo.enel.it/dettagli/",
    timeOfDayNum = 8, variableNum = 12)
}
\arguments{
  \item{location}{integer code linked to a geographical
  location in Enel DB. Check \url{meteo.enel.it} to get the
  code for your town or city}

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
  \code{enel.it} does not provide any API. Meteorological
  parameters, including temperature and irradiance, are
  scraped from its website. Therefore even slight changes
  to \code{HTML} code may break the code. The website
  provides 8 forecasts of 12 variables a day: \enumerate{
  \item "Temperatura:" (\emph{Air temperature}) [\eqn{˚C}]
  \item "Situazione:" (\emph{Current weather}) \item
  "Precipitazione:" (\emph{Precipitation}) [{"Assenti /
  Molto deboli" | "Deboli" | "Moderate" | "Abbondanti"}]
  \item "Direzione del vento:" (\emph{Wind direction})
  [{"N" | "NNE" | "NE" | "ENE" | "E" | "ESE" | "SE" | "SSE"
  | "S" | "SSO" | "SO" | "OSO" | "O" | "ONO" | "NO" |
  "NNO"}] \item "Velocità del vento (m/s):" (\emph{Wind
  speed}) [\eqn{m/s}] \item "Temperatura con vento (C°):"
  (\emph{Wind-corrected air temperature}) [\eqn{˚C}] \item
  "Calore (C°):" (\emph{Heat}) [\eqn{˚C}] \item "Umidità
  relativa (%):" (\emph{Relative humidity}) [\eqn{\%}]
  \item "Visibilità (m):" (\emph{Visibility}) [\eqn{m}]
  \item "Pressione (mbar):" (\emph{Ait pressure})
  [\eqn{mbar}] \item "Variazione della pressione:"
  (\emph{Pressure trend}) [{"-2" | "-1" | "0" | "1" | "2"}]
  \item "Irraggiamento (W/mq):" (\emph{Irradiance})
  [\eqn{W/m^2}] }
}
\note{
  Photovoltaic panel efficiency and temperature estimation
  needs air temperature and irradiance variables.
}
\author{
  Marco Bascietto \email{marco@bascietto.name}
}

