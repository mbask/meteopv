#' Scrape meteorological data from \url{meteo.enel.it}
#' 
#' 
#' \code{enel.it} does not provide any API. Meteorological parameters, including temperature and irradiance, are scraped from its website. Therefore even slight changes to \code{HTML} code may break the code.
#' A forged POST request is sent to \code{dettaglio_ajax.php}. The request includes a fake cookie, the code for the town and the date for the forecast to be looked up.
#' The website provides 8 forecasts of 12 variables a day:
#' \enumerate{
#'  \item "Te" (\emph{Air temperature}) [\eqn{^\circ C}]
#'  \item "Mc" (\emph{Current weather conditions}), one of \itemize{ \item "Sun" \item "Burning sun" \item "Scattered clouds" \item "Broken clouds" \item "Broken clouds and rain" \item "Broken clouds, rain and snow" \item "Broken clouds and light snow" \item "Overcast clouds" \item "Overcast clouds and rain" \item "Overcast clouds and rain" \item "Overcast clouds and snow" \item "Overcast clouds, snow and rain" \item "Overcast clouds, thunderstorms" \item "Overcast clouds and mist" \item "Fog" \item "Broken clouds, rain, chance of thunderstorms" \item "Overcast clouds and heavy rain" \item "Overcast clouds and heavy snow"}
#'  \item "R" (\emph{Precipitation}), one of \itemize{ \item "No rain" \item "Drizzle" \item "Light rain" \item "Moderate rain" \item "Heavy rain" \item "Very heavy rain"} according to these classes of precipitation (\eqn{mmH_2O}): \itemize{ \item \eqn{<0.1} \item \eqn{<2} \item \eqn{<6} \item \eqn{<10} \item \eqn{<15} \item \eqn{\geq 15} }
#'  \item "Wd" (\emph{Wind direction}), classes of 11.25\eqn{^\circ} each. One of \itemize{ \item "N" \item "NNE" \item "NE" \item "ENE" \item "E" \item "ESE" \item "SE" \item "SSE" \item "S" \item "SSW" \item "SW" \item "WSW" \item "W" \item "WNW" \item "NW" \item "NNW"}
#'  \item "Ws" (\emph{Wind speed}) [\eqn{m/s}]
#'  \item "Tw" (\emph{Wind-corrected air temperature (\emph{ie} Windchill)}) [\eqn{^\circ C}]
#'  \item "H" (\emph{Heat})  [\eqn{^\circ C}]
#'  \item "Rh" (\emph{Relative humidity}) [\eqn{\%}]
#'  \item "V" (\emph{Visibility}) [\eqn{m}]
#'  \item "P" (\emph{Air pressure}) [\eqn{hPa}]
#'  \item "Pt" (\emph{Pressure trend}), one of \itemize{ \item "-2" \item "-1" \item "0" \item "1" \item "2" }
#'  \item "G" (\emph{Irradiance}) [\eqn{W/m^2}]
#' }
#' 
#' @note Photovoltaic panel efficiency and temperature estimation needs air temperature and irradiance variables.
#' 
#' @param location integer code linked to a geographical location in Enel DB. Check \url{meteo.enel.it} to get the code for your town or city
#' @param dates a list of forecast dates, defaults to a list of 1 element (today date).
#' @param webAddress the \code{http} address where meteorological data is to be scraped from. Defaults to \url{http://meteo.enel.it}
#' @param timeOfDayNum how many times a day forecasts are provided. Defaults to 8, \emph{i.e.} every 3 hours.
#' @param variableLabels a vector of 12 variable names, defaults to the labels provided in Description section
#' @return a \code{data.frame} of 3 columns: \code{time}, \code{variable}, \code{value}
#' @export
#' @import lubridate
#' @import XML
#' @import RCurl
#' @author Marco Bascietto \email{marco@@bascietto.name}
#' @examples \dontrun{
#' # Scrapes today and tomorrow meteorological data for 4 italian towns
#' places = c(Roma = 170094, Nepi = 170158, Bolzano = 172858, Siracusa = 172506)
#' scrapeDate <- Sys.Date()
#' lapply(places, function(place) {measures <- scrapeMeteo(place, dates = list(scrapeDate, scrapeDate + lubridate::days(1)))})
#' }
scrapeMeteo <- function(
  location
  , dates = list(Sys.Date())
  , webAddress = "http://meteo.enel.it"
  , timeOfDayNum = 8
  , variableLabels  = c("Te", "Mc", "R", "Wd", "Ws", "Tw", "H", "Rh", "V", "P", "Pt", "G")
  ) {
  
  curlSetOpt(
    .opts = list(
      referer = paste(webAddress, "dettagli", location, sep = "/")
      , useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/536.26.17 (KHTML, like Gecko) Version/6.0.2 Safari/536.26.17"
      , cookie = "WT_FPC=id=94.37.233.237-1330998896.30256024:lv=1352757714639:ss=1352753154118"
    )
  )
  # number of variables
  variableNum <- length(variableLabels)
  # the list of keys associated to meteorological conditions
  dayInfo <- list(
    "1" = "Clear sky"
    , "2" = "Burning sun"
    , "3" = "Scattered clouds"
    , "4" = "Broken clouds"
    , "5" = "Broken clouds and rain"
    , "6" = "Broken clouds, rain and snow"
    , "7" = "Broken clouds and light snow"
    , "8" = "Overcast clouds"
    , "9" = "Overcast clouds and rain"
    , "10" = "Overcast clouds and rain"
    , "11" = "Overcast clouds and snow"
    , "12" = "Overcast clouds, snow and rain"
    , "13" = "Overcast clouds, thunderstorms"
    , "14" = "Overcast clouds and mist"
    , "15" = "Fog"
    , "16" = "Broken clouds, rain, chance of thunderstorms"
    , "17" = "Overcast clouds and heavy rain"
    , "18" = "Overcast clouds and heavy snow"
  )
  # precipitation classes
  pInfo <- list(
    "Assenti / Molto deboli" = "No rain"         # < 0.1 mm
    , "Deboli"               = "Drizzle"         # < 2.0 mm
    , "Moderate"             = "Light rain"      # < 6.0 mm
    , "Abbondanti"           = "Moderate rain"   # < 10 mm
    , "Forti"                = "Heavy rain"      # < 15 mm
    , "Molto forti"          = "Very heavy rain" # >= 15 mm
  )
  
  lapply(dates, function(date) {
  
    script <- postForm(
      uri = paste(webAddress, "dettaglio_ajax.php", sep = "/")
      , p = location
      , d = date
      , style = "POST"
    )
    doc <- htmlParse(script)
    
    # date to be looked up is transformed in POSIXct
    date <- lubridate::ymd(as.character(date), tz = "CET")
    
    # extract variable names and values
    #content_valore_title  <- xpathSApply(doc, "//div[@class='content_valore']/div[@class='content_valore_title']", xmlValue)
    content_valore_result <- trim(xpathSApply(doc, "//div[@class='content_valore']/div[@class='content_valore_result']", xmlValue))
    
    # Check whether forecasts for the desired date is available, if not return NULL
    if (is.null(content_valore_result)) {
      NULL
    } else {
      # time is a POSIXct class holding the time of each forecast. Forecasts are provided `timeOfDayNum` times each day. Time lag in seconds between forecasts is 24*60*60/timeOfDayNum (eg 86400/timeOfDayNum). Each forecast provides `variableNum` variables.
      times     <- rep(seq(date, by = 86400/timeOfDayNum, length.out = timeOfDayNum), each = variableNum)
      # Sanitize variable to strip degree symbol and replace accented a with non accented a to make it more portable
      #variables <- sub("[\u00C2\u00B0]", "", sub("[\u00E0]", "a", content_valore_title))
      # get the file path for the meorological condition
      dayIcon <- xpathSApply(doc, "//div[@class='content_valore']/div[@class='content_valore_result']/img", xmlAttrs)
      # extract the key to the meteorological condition based on a RE that matches all 1 or 2 digit numbers, assuming
      # there is only one in the URL
      dayIcon <- sapply(dayIcon, function(x) regmatches(x, gregexpr("[1]?[0-9]", x)))
      # position of the meteorological conditions in the result list
      rPosition <- seq(2, by = variableNum, length(content_valore_result))
      # replace the meteorological conditions in the proper positions in the result list
      content_valore_result[rPosition] <- sapply(dayIcon, function(x) dayInfo[[x]])
      
      # replace precipitation classes terms from italian to english
      rPosition <- rPosition + 1
      pString <- content_valore_result[rPosition]
      content_valore_result[rPosition] <- sapply(pString, function(x) pInfo[[x]])
      
      # replace W(est) in place of O(vest) occurrences in the wind direction classes
      rPosition <- rPosition + 1
      content_valore_result[rPosition] <- gsub("O", "W", content_valore_result[rPosition])

      data.frame(
        time       = times
        , variable = variableLabels
        , value    = content_valore_result
      )
    }    
  })
}
