#' Integrates periodical power values
#'
#' Given a \code{data.frame} holding power values (W)  measured of forecasted at date/time values in column \code{value}, the function integrates them into total values (kW).
#' This is a function to be invoked on groups of hourly power data on a day level.
#' Integration is performed by summing up area of trapezia formed by consecutive power values. Height of trapezia is given by the time lag between two power values, its bases are given by the two consecutive power values.
#' @param dayData a \code{data.frame} holding \code{time} and ay least 1 \code{pot} column
#' @return a summary \code{data.frame} of total power bound with columns from \code{daydata} whose name start with "year"
#' @export
#' @author Marco Bascietto \email{marco@@bascietto.name}
dailyPower <- function(dayData) {
  rows <- nrow(dayData)
  evenRow <- seq(2, rows, by = 1)
  oddRow  <- seq(1, rows-1, by = 1)
  # timeLag is a vector of trapezia heights
  timeLag <- as.numeric(with(
    dayData
    , difftime(time[evenRow], time[oddRow], units = "hours")
    ))
  # powerColumns holds columns of dayData whose name ends in [pP]ower
  powerColumns <- grep("[pP]ower$", names(dayData), value = TRUE)
  # preallocate a matrix that will hold aerea of trapezia
  tmp.power <- matrix(
    data = NA
    , nrow = rows-1
    , ncol = length(powerColumns)
    , dimnames = list(c(), powerColumns)
    )
  # Compute area of trapezia as A = 1/2 * (B+b) * h  
  tmp.power <- sapply(
    powerColumns
    , function(x) {tmp.power[,x] <- (dayData[evenRow,x] + dayData[oddRow, x]) /2 * timeLag}
    )
  # Bind and returns sum of area of trapezia / 1000 with columns from dayData whose name starts with "year"
  cbind(
    as.data.frame(as.list(colSums(tmp.power/1000)))
    , dayData[1, grep("^year", colnames(dayData), value = TRUE)]
  )
}
