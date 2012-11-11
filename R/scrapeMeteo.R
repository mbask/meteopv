scrapeMeteo <- function(place, webAddress = "http://meteo.enel.it/dettagli/", timeOfDayNum = 8, variableNum  = 12) {
  script <- getURL(paste0(webAddress, place))
  doc <- htmlParse(script)
  
  # il link di classe 'selected' è la data cui si riferisce la situazione meteo
  data <- gsub("^#", "", xpathSApply(doc, "//a[@class='selected']", xmlGetAttr, "href"))
  
  # la trasformo in POSIXct
  data <- lubridate::ymd(data)
  
  measures <- data.frame(
    time       = rep(seq(data, by = "3 hour", length.out = timeOfDayNum), each = variableNum) # replica 12 volte ciascuna delle 8 occorrenze della data, ciascuna di queste incrementata di 3 ore dalle 00:00 alle 21:00 
    , variable = trim(xpathSApply(doc, "//div[@class='content_valore']/div[@class='content_valore_title']", xmlValue))
    , value    = trim(xpathSApply(doc, "//div[@class='content_valore']/div[@class='content_valore_result']", xmlValue))
  )
  
  return(measures)
}
