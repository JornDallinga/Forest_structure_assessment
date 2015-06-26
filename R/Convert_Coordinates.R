

###UTM conversion
# Lat = Y Long = X
mydata <- read.xlsx("Coordinates individual plots meta analysis_R_inprogress.xlsx", 5)

d <- data.frame(x = mydata$Longitude, y = mydata$Latitude)
point <- SpatialPoints(data.frame(x = d$x, y = d$y))
proj4string(point) <- CRS("+proj=utm +zone=15 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
test <- spTransform(point, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
coords_output <-data.frame(coordinates(test))

d[3] <- coords_output[1]
d[4] <- coords_output[2]

write.xlsx(d, file = "outputcoordinates.xlsx", sheetName = "conversion_mexico",  append=T)



### Decimal degrees minutes conversion

## equation : decimal degrees = degrees + minutes/60 + seconds/3600
mydata1 <- read.xlsx("Coordinates individual plots meta analysis_R_inprogress.xlsx", 6)

lat <- (mydata1$Degrees_lat + (mydata1$Minutes_lat/60) + (mydata1$Seconds_lat/3600))
lon <- (mydata1$Degrees_lon + (mydata1$Minutes_lon/60) + (mydata1$Seconds_lon/3600))


dd <- data.frame(lat, lon)

write.xlsx(dd, file = "outputcoordinates.xlsx", sheetName = "conversion_mexico_Yuca_Quintana",  append=T)


### Decimal degrees minutes conversion

mydata2 <- read.xlsx("Coordinates individual plots meta analysis_R_inprogress.xlsx", 7)

df <- mydata2
test <- data.frame(do.call('rbind', strsplit(as.character(df$lat),"[Â'' \"°N]",fixed=F)))
test[2] <- test[4] 
test[3] <- test[6] 
test$X4 <- NULL
test$X5 <- NULL
test$X6 <- NULL
test$X7 <- NULL
test$X8 <- NULL
test$X9 <- NULL
test$X10 <- NULL

names(test) <- c("Degrees_lat", "Minutes_lat", "Seconds_lat")
