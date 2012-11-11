append.csv <-
function(x, file = "data/tempIrr.csv") {
  write.table(
    x = x
    , file = file
    , append = TRUE
    , sep = ","
    , col.names = FALSE
    , row.names = FALSE
  )
}
