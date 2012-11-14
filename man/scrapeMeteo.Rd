\name{scrapeMeteo}
\alias{scrapeMeteo}
\title{Scrape meteorological data from \url{meteo.enel.it}}
\usage{
  scrapeMeteo(location, dates = list(Sys.Date()),
    webAddress = "http://meteo.enel.it", timeOfDayNum = 8,
    variableNum = 12)
}
\arguments{
  \item{location}{integer code linked to a geographical
  location in Enel DB. Check \url{meteo.enel.it} to get the
  code for your town or city}

  \item{dates}{a list of forecast dates, defaults to a list
  of 1 element (today date).}

  \item{webAddress}{the \code{http} address where
  meteorological data is to be scraped from. Defaults to
  \url{http://meteo.enel.it}}

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
  to \code{HTML} code may break the code. A forged POST
  request is sent to \code{dettaglio_ajax.php}. The request
  includes a fake cookie, the code for the town and the
  date for the forecast to be looked up. The website
  provides 8 forecasts of 12 variables a day: \enumerate{
  \item "Temperatura:" (\emph{Air temperature})
  [\eqn{^\circ C}] \item "Situazione:" (\emph{Current
  weather}), this is presented as a graphical icon on the
  web page, no textual info is scraped \item
  "Precipitazione:" (\emph{Precipitation}) [\{"Assenti /
  Molto deboli" | "Deboli" | "Moderate" | "Abbondanti" |
  "Forti" | "Molto forti"\}] according to these classes of
  precipitation (\eqn{mmH_2O}): [\{\eqn{<0.1} | \eqn{<2} |
  \eqn{<6} | \eqn{<10} | \eqn{<15} | \eqn{\geq 15} \}]
  \item "Direzione del vento:" (\emph{Wind direction})
  [\{"N" | "NNE" | "NE" | "ENE" | "E" | "ESE" | "SE" |
  "SSE" | "S" | "SSO" | "SO" | "OSO" | "O" | "ONO" | "NO" |
  "NNO"\}]; each wind class is 11.25\eqn{^\circ} wide.
  \item "Velocita' del vento (m/s):" (\emph{Wind speed})
  [\eqn{m/s}] \item "Temperatura con vento
  (C\eqn{^\circ}):" (\emph{Wind-corrected air temperature})
  [\eqn{^\circ C}] \item "Calore (C\eqn{^\circ}):"
  (\emph{Heat}) [\eqn{^\circ C}] \item "Umidita' relativa
  (\eqn{\%}):" (\emph{Relative humidity}) [\eqn{\%}] \item
  "Visibilita' (m):" (\emph{Visibility}) [\eqn{m}] \item
  "Pressione (mbar):" (\emph{Air pressure}) [\eqn{hPa}]
  \item "Variazione della pressione:" (\emph{Pressure
  trend}) [\{"-2" | "-1" | "0" | "1" | "2"\}] \item
  "Irraggiamento (W/mq):" (\emph{Irradiance}) [\eqn{W/m^2}]
  }
}
\note{
  Photovoltaic panel efficiency and temperature estimation
  needs air temperature and irradiance variables.
}
\examples{
\dontrun{
library(lubridate)
# Scrapes today and tomorrow meteorological data for 4 italian towns
places = c(Roma = 170094, Nepi = 170158, Bolzano = 172858, Siracusa = 172506)
scrapeDate <- Sys.Date()
lapply(places, function(place) {measures <- scrapeMeteo(place, dates = list(scrapeDate, scrapeDate + days(1)))})
}
}
\author{
  Marco Bascietto \email{marco@bascietto.name}
}

