plotSave <-
function(fName, place) {
  ggsave(paste0("plot/", fName, place, ".pdf"), width = 29.7, height = 21, units = "cm")  
}
