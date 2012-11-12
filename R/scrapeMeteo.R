#' Scrape meteorological data from \url{meteo.enel.it}
#' 
#' 
#' \code{enel.it} does not provide any API. Meteorological parameters, including temperature and irradiance, are scraped from its website. Therefore even slight changes to \code{HTML} code may break the code.
#' The website provides 8 forecasts of 12 variables a day:
#' \enumerate{
#'  \item "Temperatura:" (\emph{Air temperature}) [\eqn{˚C}]
#'  \item "Situazione:" (\emph{Current weather})
#'  \item "Precipitazione:" (\emph{Precipitation}) [{"Assenti / Molto deboli" | "Deboli" | "Moderate" | "Abbondanti"}]
#'  \item "Direzione del vento:" (\emph{Wind direction}) [{"N" | "NNE" | "NE" | "ENE" | "E" | "ESE" | "SE" | "SSE" | "S" | "SSO" | "SO" | "OSO" | "O" | "ONO" | "NO" | "NNO"}]
#'  \item "Velocità del vento (m/s):" (\emph{Wind speed}) [\eqn{m/s}]
#'  \item "Temperatura con vento (C°):" (\emph{Wind-corrected air temperature}) [\eqn{˚C}]
#'  \item "Calore (C°):" (\emph{Heat})  [\eqn{˚C}]
#'  \item "Umidità relativa (%):" (\emph{Relative humidity}) [\eqn{\%}]
#'  \item "Visibilità (m):" (\emph{Visibility}) [\eqn{m}]
#'  \item "Pressione (mbar):" (\emph{Air pressure}) [\eqn{hPa}]
#'  \item "Variazione della pressione:" (\emph{Pressure trend}) [{"-2" | "-1" | "0" | "1" | "2"}]
#'  \item "Irraggiamento (W/mq):" (\emph{Irradiance}) [\eqn{W/m^2}]
#' }
#' 
#' @note Photovoltaic panel efficiency and temperature estimation needs air temperature and irradiance variables.
#' 
#' @param location integer code linked to a geographical location in Enel DB. Check \url{meteo.enel.it} to get the code for your town or city
#' @param webAddress the \code{http} address where meteorological data is to be scraped from. Defaults to \url{http://meteo.enel.it/dettagli/}
#' @param timeOfDayNum how many times a day forecasts are provided. Defaults to 8, \emph{i.e.} every 3 hours.
#' @param variableNum how many variables each forects provide. Defaults to 12.
#' @return a \code{data.frame} of 3 columns: \code{time}, \code{variable}, \code{value}
#' @export
#' @import lubridate
#' @import XML
#' @import RCurl
#' @author Marco Bascietto \email{marco@@bascietto.name}
scrapeMeteo <- function(
  location
  , webAddress = "http://meteo.enel.it/dettagli/"
  , timeOfDayNum = 8
  , variableNum  = 12
  ) {
  
  script <- getURL(paste0(webAddress, location))
  doc <- htmlParse(script)
  
  # il link di classe 'selected' è la data cui si riferisce la situazione meteo odierna
  data <- gsub("^#", "", xpathSApply(doc, "//a[@class='selected']", xmlGetAttr, "href"))
  
  # la trasformo in POSIXct
  data <- lubridate::ymd(data)
  
  data.frame(
    # time is a POSIXct class holding the time of each forecast. Forecasts are provided `timeOfDayNum` times each day. Time lag in seconds between forecasts is 24*60*60/timeOfDayNum (eg 86400/timeOfDayNum). Each forecast provides `variableNum` variables.
    time       = rep(seq(data, by = 86400/timeOfDayNum, length.out = timeOfDayNum), each = variableNum)
    , variable = trim(xpathSApply(doc, "//div[@class='content_valore']/div[@class='content_valore_title']", xmlValue))
    , value    = trim(xpathSApply(doc, "//div[@class='content_valore']/div[@class='content_valore_result']", xmlValue))
  )

}
