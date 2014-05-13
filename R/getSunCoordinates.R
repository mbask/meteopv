astronomersAlmanacTime <- function(x) {
  # Astronomer's almanach time is the number of 
  # days since (noon, 1 January 2000)
  origin <- as.POSIXct("2000-01-01 12:00:00")
  as.numeric(difftime(x, origin, units = "days"))
}

hourOfDay <- function(x) {
  x <- as.POSIXlt(x)
  with(x, hour + min / 60 + sec / 3600)
}

degreesToRadians <- function(degrees) {
  degrees * pi / 180
}

radiansToDegrees <- function(radians) {
  radians * 180 / pi
}

meanLongitudeDegrees <- function(time) {
  (280.460 + 0.9856474 * time) %% 360
}

meanAnomalyRadians <- function(time) {
  degreesToRadians((357.528 + 0.9856003 * time) %% 360)
}

eclipticLongitudeRadians <- function(mnlong, mnanom) {
  degreesToRadians(
    (mnlong + 1.915 * sin(mnanom) + 0.020 * sin(2 * mnanom)) %% 360
  )
}

eclipticObliquityRadians <- function(time) {
  degreesToRadians(23.439 - 0.0000004 * time)
}

rightAscensionRadians <- function(oblqec, eclong) {
  num <- cos(oblqec) * sin(eclong)
  den <- cos(eclong)
  ra <- atan(num / den)
  ra[den < 0] <- ra[den < 0] + pi
  ra[den >= 0 & num < 0] <- ra[den >= 0 & num < 0] + 2 * pi 
  ra
}

rightDeclinationRadians <- function(oblqec, eclong) {
  asin(sin(oblqec) * sin(eclong))
}

greenwichMeanSiderealTimeHours <- function(time, hour) {
  (6.697375 + 0.0657098242 * time + hour) %% 24
}

localMeanSiderealTimeRadians <- function(gmst, long) {
  degreesToRadians(15 * ((gmst + long / 15) %% 24))
}

hourAngleRadians <- function(lmst, ra) {
  ((lmst - ra + pi) %% (2 * pi)) - pi
}

elevationRadians <- function(lat, dec, ha) {
  asin(sin(dec) * sin(lat) + cos(dec) * cos(lat) * cos(ha))
}

solarAzimuthRadians <- function(lat, dec, ha, el) {
  az <- asin(-cos(dec) * sin(ha) / cos(el))
  cosAzPos <- (0 <= sin(dec) - sin(el) * sin(lat))
  sinAzNeg <- (sin(az) < 0)
  az[cosAzPos & sinAzNeg] <- az[cosAzPos & sinAzNeg] + 2 * pi
  az[!cosAzPos] <- pi - az[!cosAzPos]
  az
}


#' Estimates Sun azimuth and height above a location at a given time
#' 
#' @param locCoordinates a vector holding longitude and latitude of a single location on earth
#' @param time a POSIXct class object
#' @return a matrix of 2 columns, \code{azimuth} and \code{height} of sun
#' @export
#' @author Marco Bascietto \email{marco@@bascietto.name}
#' @references Code adapted from \url{http://stackoverflow.com/questions/8708048/position-of-the-sun-given-time-of-day-latitude-and-longitude}, Richie Cotton answer
getSunCoordinates <- function(locCoordinates = c(6.5, 46.5), time = Sys.time()) {
  hour <- hourOfDay(time)
  time <- astronomersAlmanacTime(time)
  
  # Ecliptic coordinates  
  mnlong <- meanLongitudeDegrees(time)   
  mnanom <- meanAnomalyRadians(time)  
  eclong <- eclipticLongitudeRadians(mnlong, mnanom)     
  oblqec <- eclipticObliquityRadians(time)
  
  # Celestial coordinates
  ra <- rightAscensionRadians(oblqec, eclong)
  dec <- rightDeclinationRadians(oblqec, eclong)
  
  # Local coordinates
  gmst <- greenwichMeanSiderealTimeHours(time, hour)  
  lmst <- localMeanSiderealTimeRadians(gmst, locCoordinates[1]) # longitude
  
  # Hour angle
  ha <- hourAngleRadians(lmst, ra)
  
  # Latitude to radians
  lat <- degreesToRadians(locCoordinates[2]) # latitude
  
  # Azimuth and elevation
  el <- elevationRadians(lat, dec, ha)
  az <- solarAzimuthRadians(lat, dec, ha, el)

  tmp <- matrix(
    c(radiansToDegrees(el), radiansToDegrees(az))
    , byrow = TRUE
    , ncol = 2
    )
  colnames(tmp) <- c("height", "azimuth")

  return(tmp)
}