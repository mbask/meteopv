#' Estimates Sun azimuth and height above a location
#' 
#' It is just a wrapper around maptools::solarpos
#' 
#' @param time a POSIXct class object
#' @param locCoordinates a vector holding longitude and latitude of a single location on earth
#' @return a matrix of 2 columns, "\code{azimuth}" and "\code{height}" of sun
#' @import maptools
#' @export
#' @author Marco Bascietto \email{marco@@bascietto.name}
getSunCoordinates <- function(locCoordinates, time) {
  tmp <- maptools::solarpos(locCoordinates, time)
  colnames(tmp) <- c("azimuth", "height")
  return(tmp)
}