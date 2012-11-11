#' @title Trim leading and trailing spaces from a string
#' 
#' @param x the string to be cleaned
#' @return a string with leading and trailing spaces removed

#' @author Marco Bascietto \email{marco@@bascietto.name}
trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}
