#' Appends data to a CSV file or connection
#' 
#' \code{x} is appended to a csv file (if it already exists) or written to a new file.
#' Calls internally \code{write.table}.
#' 
#' @param x a \code{data.frame} to be appended to an existing CSV file
#' @param file CSV file name. Default value "\code{data/tempIrr.csv}"
#' @export
#' @seealso \code{\link{write.table}}
#' @author Marco Bascietto \email{marco@@bascietto.name}
append.csv <- function(
  x
  , file = "data/tempIrr.csv"
  ) {
  if (file.exists(file)) {
    write.table(
      x = x
      , file = file
      , append = TRUE
      , sep = ","
      , col.names = FALSE
      , row.names = FALSE
    )
  } else {
    write.table(
      x = x
      , file = file
      , sep = ","
      , row.names = FALSE
      )
  }
}
