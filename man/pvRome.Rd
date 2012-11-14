\docType{data}
\name{pvRome}
\alias{pvRome}
\title{Sun irradiance and air temperature in Rome}
\description{
  Sun irradiance and air temperature in Rome from October
  2012 to November 2012 as scraped daily from
  \url{meteo.enel.it}.
}
\examples{
library(reshape2)
library(lubridate)
library(meteopv)
library(plyr)
library(ggplot2)

data(pvRome)

cfg <- list(
 gamma              = 0.4 # \%/degree Celsius A typical polycristalline cell power coefficient
 , etaStd           = 15  # \%  A typical polycristalline cell efficiency
 , NOCT             = 45  # degree Celsius A typical polycristalline cell Nominal Operating Cell Temperature
 , supPannello1kWp  = 7 # m^2 A typical polycristalline cell area for 1kWp system
 , PVlosses         = 3+14 # \% An estimate of PV system losses (Photovoltaic Geographical Information System, European #'Commission Joint Research Centre Ispra, Italy)
 , tilt             = 18 # degree Roof tilt
 , geoCoord         = matrix(
   c(12.44, 41.79) # Longitude and latitude of...
   , byrow = TRUE
   , ncol = 2
   , dimnames = list(c("Rome"))
 )
)

pvRome <- within(pvRome, {
 # Convert value to from factor to numeric
 value    <- as.numeric(as.character(value))
 # Rename and convert variable to factor
 variable <- factor(ifelse(variable == "Temperatura:", "Te", "G"))
 # Convert time from factor to POSIXct class
 time <- ymd_hms(time, tz = "CET")
 # Add location info
 place <- factor("Rome")
})

pvRome <- dcast(pvRome, time + place ~ variable)

pvRome <- getPVEfficiency(pvRome, cfg)

pvRome <- within(pvRome, {
 day <- yday(time)
 dayHourOfMonth <- mday(time) + hour(time) / 24
 yearMonth  <- month(time, label = TRUE, abbr = FALSE)
})

pvRome.m <- melt(
 pvRome
 , id.vars = c("time", "G", "dayHourOfMonth", "yearMonth", "place")
 , variable.name = "variable"
)

ggplot(pvRome.m[grep("^T[ec]$", pvRome.m$variable), ]) +
 geom_line(aes(x = dayHourOfMonth, y = value, color = variable)) +
 labs(list(title = "Hourly temperature", y = "Temperature (degree C)", x = "Day of month")) +
 guides(colour = guide_legend("Temperature")) +
 theme_bw() +
 facet_wrap(~yearMonth)
}
\author{
  Marco Bascietto \email{marco@bascietto.name}
}
\keyword{data}
\keyword{datasets}

