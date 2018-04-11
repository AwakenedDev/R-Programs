# @author Jigar D. Prajapati

#Max temp reported by any WBAN's during August
# @param weatherDF
# @return list that has information on the max temp
maxTemp <- function(weatherDF) {
  tMaxPosition <- which.max(weatherDF$V3) #One simple method computes the max
  
  tDate <- as.Date(as.character(weatherDF$V2[[tMaxPosition]]), "%Y%m%d")
  tDateFinal <- toString(format(tDate, format="%B %d %Y")) #date formating
  
  tMax <- toString(weatherDF$V3[[tMaxPosition]])
  stationName <- toString(weatherDF$V6[[tMaxPosition]])
  locationName <- toString(weatherDF$V7[[tMaxPosition]])
  
  return(c(tMax, tDateFinal, stationName, locationName))
}

#Min temp reported by any WBAN's during August
# @param weatherDF
# @return list that has information on the min temp
minTemp <- function(weatherDF) {
  tMinPosition <- which.min(weatherDF$V4) #One simple method computes the min
  
  tDate <- as.Date(as.character(weatherDF$V2[[tMinPosition]]), "%Y%m%d")
  tDateFinal <- toString(format(tDate, format="%B %d %Y")) #date formating
  
  tMin <- toString(weatherDF$V4[[tMinPosition]])
  stationName <- toString(weatherDF$V6[[tMinPosition]])
  locationName <- toString(weatherDF$V7[[tMinPosition]])
  
  return(c(tMin, tDateFinal, stationName, locationName))
}

#The average average (Tavg) temperature 
# @param weatherDF
# @return the value of average TAvg
avgTemp <- function(weatherDF) {
  avgTemp <- mean(weatherDF$V5)
  return(avgTemp)
}

#The hottest day in Pennsylvania in August
# @param weatherDF
# @return the list that has information of the hottest day in Pennsylvania
hotDay <- function(weatherDF) {
  weatherDFCopy <- weatherDF
  combined <-aggregate(V3 ~ V2, weatherDF, mean) #One simple methods groups all similar days and performs mean on the values grouped
  
  #plot simple graph for hot temperatures (red)
  plot(combined$V3, main="Average TMax for each day", xlab = "Day", ylab = "AvgTmax", col="red", pch = 19, xlim=c(1,31))
  
  hotMaxPosition <- which.max(combined$V3) #then find max of the grouped data
  
  hotDate <- as.Date(as.character(combined$V2[[hotMaxPosition]]), "%Y%m%d")
  hotDateFinal <- toString(format(hotDate, format="%B %d %Y"))
  avgTMax <- toString(combined$V3[[hotMaxPosition]])
  
  return(c(hotDateFinal, avgTMax))
  
}

#The coldest day in Pennsylvania in August
# @param weatherDF
# @return the list that has information of the coldest day in Pennsylvania
coldDay <- function(weatherDF) {
  weatherDFCopy <- weatherDF
  combined <- aggregate(V4 ~ V2, weatherDF, mean)#One simple methods groups all similar days and performs mean on the values grouped
  
  #plot simple graph for cold temperatures (blue)
  plot(combined$V4, main="Average TMin for each day", xlab = "Day", ylab = "AvgTmin", col="blue", pch = 19, xlim=c(1,31))
  
  coldMinPosition <- which.min(combined$V4) #then find min of the grouped data
  
  coldDate <- as.Date(as.character(combined$V2[[coldMinPosition]]), "%Y%m%d")
  coldDateFinal <- toString(format(coldDate, format="%B %d %Y"))
  avgTMin <- toString(combined$V4[[coldMinPosition]])
  
  return(c(coldDateFinal, avgTMin))
  
}

main <- function() {
  tryCatch({
    cat(sprintf("\nPlease select the data file.\n"))
    response <- file.choose() #Ask user to choose the file interactively
    cat(sprintf("\nSelected %s\n", response))
    
    weatherData = read.table(response, header = FALSE, sep = " ") #read .txt file to data frame.
    
    cat(sprintf("\n* * * * PA Weather Statistics * * * *\n"))
    
    #outputs maxTemp info
    maxT <- maxTemp(weatherData)
    cat(sprintf("\n1. Maximum temperature reported by any of the WBAN's is "))
    cat(sprintf("\n%-12s %-15s %-20s %-30s", "MaxTemp(F)", "Date", "Station", "Location"))
    cat(sprintf("\n--------------------------------------------------------------------------------------------"))
    cat(sprintf("\n%-12i %-15s %-20s %-30s\n", as.integer(maxT[[1]]), maxT[[2]], maxT[[3]], maxT[[4]]))
    
    #outputs minTemp info
    minT <- minTemp(weatherData)
    cat(sprintf("\n2. Minimum temperature reported by any of the WBAN's is "))
    cat(sprintf("\n%-12s %-15s %-20s %-30s", "MinTemp(F)", "Date", "Station", "Location"))
    cat(sprintf("\n--------------------------------------------------------------------------------------------"))
    cat(sprintf("\n%-12i %-15s %-20s %-30s\n", as.integer(minT[[1]]), minT[[2]], minT[[3]], minT[[4]]))
    
    #outputs Average Temp info
    tAvg <- avgTemp(weatherData)
    cat(sprintf("\n3. Average average (Tavg) temperature was %.2f Fahrenheit\n", tAvg))
    
    #outputs Hottest Day info
    avgHot <- hotDay(weatherData)
    cat(sprintf("\n4. The hottest day in Pennsylvania was on %s with average max temperature of %.2f Fahrenheit\n", avgHot[[1]], as.double(avgHot[[2]])))
    
    #outputs Coldest Day info
    avgCold <- coldDay(weatherData)
    cat(sprintf("\n5. The coldest day in Pennsylvania was on %s with average min temperature of %.2f Fahrenheit\n", avgCold[[1]], as.double(avgCold[[2]])))
    
    cat(sprintf("\n* * * * End of PA Weather Statistics * * * *\n"))
    
  }, error = function(e) {
    cat(paste("Oops!", e, sep=" "))
  })
}

main() #call main when program runs
