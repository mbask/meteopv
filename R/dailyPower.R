dailyPower <-
function(dayData) {
  rows <- nrow(dayData)
  evenRow <- seq(2, rows, by = 1)
  oddRow  <- seq(1, rows-1, by = 1)
  timeLag <- as.numeric(with(dayData, difftime(time[evenRow], time[oddRow], units = "hours"))) # altezza del trapezio
  powerColumns <- grep("^pot", names(dayData), value = TRUE)
  tmp.power <- matrix(data = NA, nrow = rows-1, ncol = length(powerColumns), dimnames = list(c(), powerColumns))
  tmp.power <- sapply(
    powerColumns
    , function(x) {tmp.power[,x] <- (dayData[evenRow,x] + dayData[oddRow, x]) /2 * timeLag}
    )
  cbind(
    as.data.frame(as.list(colSums(tmp.power/1000)))
    , dayData[1, grep("^year", colnames(dayData), value = TRUE)]
  )
}
