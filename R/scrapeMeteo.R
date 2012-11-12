#' @title Scrape meteorological data from \url{meteo.enel.it}
#' 
#' \code{enel.it} does not provide any API. Meteorological parameters, including temperature and irradiance, are scraped from its website. Therefore even sligh changes to \code{HTML} code may break the code.
#' 
#' @param place integer code linked to a geographical location in Enel DB. Check \url{meteo.enel.it}{meteo.enel.it} to get the code for your town or city
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
  place
  , webAddress = "http://meteo.enel.it/dettagli/"
  , timeOfDayNum = 8
  , variableNum  = 12
  ) {
  
  script <- getURL(paste0(webAddress, place))
  doc <- htmlParse(script)
  
  # il link di classe 'selected' è la data cui si riferisce la situazione meteo odierna
  data <- gsub("^#", "", xpathSApply(doc, "//a[@class='selected']", xmlGetAttr, "href"))
  
  # la trasformo in POSIXct
  data <- lubridate::ymd(data)
  
  measures <- data.frame(
    # time is a POSIXct class holding the time of each forecast. Forecasts are provided `timeOfDayNum` times each day. Time lag in seconds between forecasts is 24/timeOfDayNum*60 (eg 0.4/timeOfDayNum). Each forecast provides `variableNum` variables.
    time       = rep(seq(data, by = 0.4/timeOfDayNum, length.out = timeOfDayNum), each = variableNum)
    , variable = trim(xpathSApply(doc, "//div[@class='content_valore']/div[@class='content_valore_title']", xmlValue))
    , value    = trim(xpathSApply(doc, "//div[@class='content_valore']/div[@class='content_valore_result']", xmlValue))
  )
  
  return(measures)
}
