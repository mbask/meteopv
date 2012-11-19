\name{scrapeMeteo}
\alias{scrapeMeteo}
\title{Scrape meteorological data from \url{meteo.enel.it}}
\usage{
  scrapeMeteo(location, dates = list(Sys.Date()),
    webAddress = "http://meteo.enel.it", timeOfDayNum = 8,
    variableLabels = c("Te", "WeatherTheme", "P", "Wd", "Ws", "Tw", "H", "Rh", "V", "P", "Pt", "G"))
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

  \item{variableLabels}{a vector of 12 variable names,
  defaults to the labels provided in Description section}
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
  \item "Te" (\emph{Air temperature}) [\eqn{^\circ C}]
  \item "Weather theme" (\emph{Current weather}), this is
  presented as a graphical icon on the web page, no textual
  info is scraped \item "P" (\emph{Precipitation})
  [\{"Assenti / Molto deboli" | "Deboli" | "Moderate" |
  "Abbondanti" | "Forti" | "Molto forti"\}] according to
  these classes of precipitation (\eqn{mmH_2O}):
  [\{\eqn{<0.1} | \eqn{<2} | \eqn{<6} | \eqn{<10} |
  \eqn{<15} | \eqn{\geq 15} \}] \item "Wd" (\emph{Wind
  direction}) [\{"N" | "NNE" | "NE" | "ENE" | "E" | "ESE" |
  "SE" | "SSE" | "S" | "SSO" | "SO" | "OSO" | "O" | "ONO" |
  "NO" | "NNO"\}]; each wind class is 11.25\eqn{^\circ}
  wide.  \item "Ws" (\emph{Wind speed}) [\eqn{m/s}] \item
  "Tw" (\emph{Wind-corrected air temperature (\emph{ie}
  Windchill)}) [\eqn{^\circ C}] \item "H" (\emph{Heat})
  [\eqn{^\circ C}] \item "Rh" (\emph{Relative humidity})
  [\eqn{\%}] \item "V" (\emph{Visibility}) [\eqn{m}] \item
  "P" (\emph{Air pressure}) [\eqn{hPa}] \item "Pt"
  (\emph{Pressure trend}) [\{"-2" | "-1" | "0" | "1" |
  "2"\}] \item "G" (\emph{Irradiance}) [\eqn{W/m^2}] }
}
\note{
  Photovoltaic panel efficiency and temperature estimation
  needs air temperature and irradiance variables.
}
\examples{
\dontrun{
# Scrapes today and tomorrow meteorological data for 4 italian towns
places = c(Roma = 170094, Nepi = 170158, Bolzano = 172858, Siracusa = 172506)
scrapeDate <- Sys.Date()
lapply(places, function(place) {measures <- scrapeMeteo(place, dates = list(scrapeDate, scrapeDate + lubridate::days(1)))})
}
}
\author{
  Marco Bascietto \email{marco@bascietto.name}
}

