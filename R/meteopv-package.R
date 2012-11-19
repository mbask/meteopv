#' Scraping from the Web the hourly solar irradicance and air temperature to estimate efficiency and energy produced by photovoltaic cell systems.
#'
#' A simpe example of meteorological data elaboration is given in \code{\link{pvRome}} dataset.
#' 
#' @name meteopv-package
#' @aliases meteopv
#' @docType package
#' @author Marco Bascietto \email{marco@@bascietto.name}
#' @keywords package
#' @references Source code is hosted at GitHub (\url{http://mbask.github.com/meteopv/})
NULL

#' Sun irradiance and air temperature in Rome
#' 
#' Sun irradiance and air temperature in Rome from October 2012 to November 2012 as scraped daily from \url{meteo.enel.it}.
#'
#' @name pvRome
#' @docType data
#' @author Marco Bascietto \email{marco@@bascietto.name}
#' @keywords data
#' @examples
#' library(reshape2)
#' library(lubridate)
#' library(ggplot2)
#' data(pvRome)
#' cfg <- list(
#'   gamma              = 0.4 # %/degree Celsius A typical polycristalline cell power coefficient
#'   , etaStd           = 15  # %  A typical polycristalline cell efficiency
#'   , NOCT             = 45  # degree Celsius A typical polycristalline cell Nominal Operating Cell Temperature
#'   , supPannello1kWp  = 7 # m^2 A typical polycristalline cell area for 1kWp system
#'   , PVlosses         = 3+14 # % An estimate of PV system losses (Photovoltaic Geographical Information System, European Commission Joint Research Centre Ispra, Italy)
#'   , tilt             = 18 # degree Roof tilt
#'   , geoCoord         = matrix(
#'     c(12.44, 41.79) # Longitude and latitude of...
#'     , byrow = TRUE
#'     , ncol = 2
#'     , dimnames = list(c("Rome")) 
#'   )
#' )
#' # We only need air temperature and sun irradiance in order to estimate PV efficiency
#' pvRome <- pvRome[pvRome$variable %in% c("Te", "G"),]
#' pvRome <- within(pvRome, {
#'   # Strip unused factors
#'   variable <- factor(variable)
#'   # Convert value to from factor to numeric
#'   value    <- as.numeric(as.character(value))
#'   # Convert time from factor to POSIXct class
#'   time <- ymd_hms(time, tz = "CET")
#'   # Add location info
#'   place <- factor("Rome")
#' })
#' 
#' pvRome <- dcast(pvRome, time + place ~ variable)
#' 
#' pvRome <- getPVEfficiency(pvRome, cfg)
#' 
#' pvRome <- within(pvRome, {
#'   dayHourOfMonth <- mday(time) + hour(time) / 24
#'   yearMonth  <- month(time, label = TRUE, abbr = FALSE)
#' })
#' 
#' pvRome.m <- melt(
#'   pvRome
#'   , id.vars = c("time", "G", "dayHourOfMonth", "yearMonth", "place")
#'   , variable.name = "variable"
#' )
#' 
#' TePlot <- ggplot(pvRome.m[pvRome.m$variable == "Te", ]) +
#'   geom_line(aes(x = dayHourOfMonth, y = value, color = yearMonth, group = yearMonth)) +
#'   labs(list(title = "Hourly air temperature", y = "Temperature (degree C)", x = "Day of month")) +
#'   guides(colour = guide_legend("Temperature")) +
#'   theme_bw()
#' TcPlot <- TePlot %+% pvRome.m[pvRome.m$variable == "Tc", ] +
#'   labs(title = "Hourly PV cell temperature")
#' plot(TePlot)
#' plot(TcPlot)
NULL