
maxTemp <- function(weatherDF) {
  tMaxPosition <- which.max(weatherDF$V3)
  
  tDate <- as.Date(as.character(weatherDF$V2[[tMaxPosition]]), "%Y%m%d")
  tDateFinal <- toString(format(tDate, format="%B %d %Y"))
  
  tMax <- toString(weatherDF$V3[[tMaxPosition]])
  stationName <- toString(weatherDF$V6[[tMaxPosition]])
  locationName <- toString(weatherDF$V7[[tMaxPosition]])
  
  return(c(tMax, tDateFinal, stationName, locationName))
}

minTemp <- function(weatherDF) {
  tMinPosition <- which.min(weatherDF$V4)
  
  tDate <- as.Date(as.character(weatherDF$V2[[tMinPosition]]), "%Y%m%d")
  tDateFinal <- toString(format(tDate, format="%B %d %Y"))
  
  tMin <- toString(weatherDF$V4[[tMinPosition]])
  stationName <- toString(weatherDF$V6[[tMinPosition]])
  locationName <- toString(weatherDF$V7[[tMinPosition]])
  
  return(c(tMin, tDateFinal, stationName, locationName))
}

avgTemp <- function(weatherDF) {
  avgTemp <- mean(weatherDF$V5)
  return(avgTemp)
}

hotDay <- function(weatherDF) {
  weatherDFCopy <- weatherDF
  combined <-aggregate(V3 ~ V2, weatherDF, mean)
  
  #plot simple graph for hot temperatures (red)
  plot(combined$V3, main="Average TMax for each day", xlab = "Day", ylab = "AvgTmax", col="red", pch = 19, xlim=c(1,31))
  
  hotMaxPosition <- which.max(combined$V3)
  
  hotDate <- as.Date(as.character(combined$V2[[hotMaxPosition]]), "%Y%m%d")
  hotDateFinal <- toString(format(hotDate, format="%B %d %Y"))
  avgTMax <- toString(combined$V3[[hotMaxPosition]])
  
  return(c(hotDateFinal, avgTMax))
  
}

coldDay <- function(weatherDF) {
  weatherDFCopy <- weatherDF
  combined <- aggregate(V4 ~ V2, weatherDF, mean)
  
  #plot simple graph for cold temperatures (blue)
  plot(combined$V4, main="Average TMin for each day", xlab = "Day", ylab = "AvgTmin", col="blue", pch = 19, xlim=c(1,31))
  
  coldMinPosition <- which.min(combined$V4)
  
  coldDate <- as.Date(as.character(combined$V2[[coldMinPosition]]), "%Y%m%d")
  coldDateFinal <- toString(format(coldDate, format="%B %d %Y"))
  avgTMin <- toString(combined$V4[[coldMinPosition]])
  
  return(c(coldDateFinal, avgTMin))
  
}

main <- function() {
  tryCatch({
    cat(sprintf("\nPlease select the data file.\n"))
    response <- file.choose()
    cat(sprintf("\nSelected %s\n", response))
    weatherData = read.table(response, header = FALSE, sep = " ")
    options(max.print = 10000)
    #print(weatherData)
    
    maxT <- maxTemp(weatherData)
    cat(sprintf("\n1. Maximum temperature reported by any of the WBAN's is "))
    cat(sprintf("\n%-12s %-15s %-20s %-30s", "MaxTemp(F)", "Date", "Station", "Location"))
    cat(sprintf("\n--------------------------------------------------------------------------------------------"))
    cat(sprintf("\n%-12i %-15s %-20s %-30s\n", as.integer(maxT[[1]]), maxT[[2]], maxT[[3]], maxT[[4]]))
    
    minT <- minTemp(weatherData)
    cat(sprintf("\n2. Minimum temperature reported by any of the WBAN's is "))
    cat(sprintf("\n%-12s %-15s %-20s %-30s", "MinTemp(F)", "Date", "Station", "Location"))
    cat(sprintf("\n--------------------------------------------------------------------------------------------"))
    cat(sprintf("\n%-12i %-15s %-20s %-30s\n", as.integer(minT[[1]]), minT[[2]], minT[[3]], minT[[4]]))
    
    tAvg <- avgTemp(weatherData)
    cat(sprintf("\n3. Average average (Tavg) temperature was %.2f Fahrenheit\n", tAvg))
    
    avgHot <- hotDay(weatherData)
    cat(sprintf("\n4. The hottest day in Pennsylvania was on %s with average max temperature of %.2f Fahrenheit\n", avgHot[[1]], as.double(avgHot[[2]])))
    
    avgCold <- coldDay(weatherData)
    cat(sprintf("\n5. The coldest day in Pennsylvania was on %s with average min temperature of %.2f Fahrenheit\n", avgCold[[1]], as.double(avgCold[[2]])))
    
  }, error = function(e) {
    cat(paste("Oops!", e, sep=" "))
  })
}

main()