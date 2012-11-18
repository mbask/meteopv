#' Scrape meteorological data from \url{meteo.enel.it}
#' 
#' 
#' \code{enel.it} does not provide any API. Meteorological parameters, including temperature and irradiance, are scraped from its website. Therefore even slight changes to \code{HTML} code may break the code.
#' A forged POST request is sent to \code{dettaglio_ajax.php}. The request includes a fake cookie, the code for the town and the date for the forecast to be looked up.
#' The website provides 8 forecasts of 12 variables a day:
#' \enumerate{
#'  \item "Te" (\emph{Air temperature}) [\eqn{^\circ C}]
#'  \item "Weather theme" (\emph{Current weather}), this is presented as a graphical icon on the web page, no textual info is scraped
#'  \item "P" (\emph{Precipitation}) [\{"Assenti / Molto deboli" | "Deboli" | "Moderate" | "Abbondanti" | "Forti" | "Molto forti"\}] according to these classes of precipitation (\eqn{mmH_2O}): [\{\eqn{<0.1} | \eqn{<2} | \eqn{<6} | \eqn{<10} | \eqn{<15} | \eqn{\geq 15} \}]
#'  \item "Wd" (\emph{Wind direction}) [\{"N" | "NNE" | "NE" | "ENE" | "E" | "ESE" | "SE" | "SSE" | "S" | "SSO" | "SO" | "OSO" | "O" | "ONO" | "NO" | "NNO"\}]; each wind class is 11.25\eqn{^\circ} wide.
#'  \item "Ws" (\emph{Wind speed}) [\eqn{m/s}]
#'  \item "WTe" (\emph{Wind-corrected air temperature}) [\eqn{^\circ C}]
#'  \item "Heat" (\emph{Heat})  [\eqn{^\circ C}]
#'  \item "Rh" (\emph{Relative humidity}) [\eqn{\%}]
#'  \item "V" (\emph{Visibility}) [\eqn{m}]
#'  \item "P" (\emph{Air pressure}) [\eqn{hPa}]
#'  \item "Pt" (\emph{Pressure trend}) [\{"-2" | "-1" | "0" | "1" | "2"\}]
#'  \item "G" (\emph{Irradiance}) [\eqn{W/m^2}]
#' }
#' 
#' @note Photovoltaic panel efficiency and temperature estimation needs air temperature and irradiance variables.
#' 
#' @param location integer code linked to a geographical location in Enel DB. Check \url{meteo.enel.it} to get the code for your town or city
#' @param dates a list of forecast dates, defaults to a list of 1 element (today date).
#' @param webAddress the \code{http} address where meteorological data is to be scraped from. Defaults to \url{http://meteo.enel.it}
#' @param timeOfDayNum how many times a day forecasts are provided. Defaults to 8, \emph{i.e.} every 3 hours.
#' @param variableNum how many variables each forects provide. Defaults to 12.
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
  , variableNum  = 12
  ) {
  
  curlSetOpt(
    .opts = list(
      referer = paste(webAddress, "dettagli", location, sep = "/")
      , useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/536.26.17 (KHTML, like Gecko) Version/6.0.2 Safari/536.26.17"
      , cookie = "WT_FPC=id=94.37.233.237-1330998896.30256024:lv=1352757714639:ss=1352753154118"
    )
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
    content_valore_title  <- xpathSApply(doc, "//div[@class='content_valore']/div[@class='content_valore_title']", xmlValue)
    content_valore_result <- xpathSApply(doc, "//div[@class='content_valore']/div[@class='content_valore_result']", xmlValue)
    
    # Check whether forecasts for the desired date is available, if not return NULL
    if (is.null(content_valore_title)) {
      NULL
    } else {
      # time is a POSIXct class holding the time of each forecast. Forecasts are provided `timeOfDayNum` times each day. Time lag in seconds between forecasts is 24*60*60/timeOfDayNum (eg 86400/timeOfDayNum). Each forecast provides `variableNum` variables.
      times     <- rep(seq(date, by = 86400/timeOfDayNum, length.out = timeOfDayNum), each = variableNum)
      # Sanitize variable to strip degree symbol and replace accented a with non accented a to make it more portable
      #variables <- sub("[\u00C2\u00B0]", "", sub("[\u00E0]", "a", content_valore_title))

      data.frame(
        time       = times
        , variable = c("Te", "WeatherTheme", "P", "Wd", "Ws", "WTe", "Heat", "Rh", "V", "P", "Pt", "G")
        , value    = trim(content_valore_result)
      )
    }    
  })
}
