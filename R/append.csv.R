#' @title Appends data to a CSV file or connection
#' 
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
  write.table(
    x = x
    , file = file
    , append = TRUE
    , sep = ","
    , col.names = FALSE
    , row.names = FALSE
  )
}
