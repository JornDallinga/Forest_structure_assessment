

###UTM conversion
# Lat = Y Long = X
mydata <- read.xlsx("Planas.xls", "21M")

d <- data.frame(y = mydata$Longitude, x = mydata$Latitude)
point <- SpatialPoints(data.frame(x = d$x, y = d$y))
proj4string(point) <- CRS("+proj=utm +zone=20 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
test <- spTransform(point, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
coords_output <-data.frame(coordinates(test))

d[3] <- coords_output[1]
d[4] <- coords_output[2]

write.xlsx(d, file = "outputcoordinates.xlsx", sheetName = "conversion_BRA_Manaus2",  append=T)



### Decimal degrees minutes conversion

## equation : decimal degrees = degrees + minutes/60 + seconds/3600
mydata1 <- read.xlsx("Coordinates individual plots meta analysis_R_inprogress.xlsx", 6)

lat <- (mydata1$Degrees_lat + (mydata1$Minutes_lat/60) + (mydata1$Seconds_lat/3600))
lon <- (mydata1$Degrees_lon + (mydata1$Minutes_lon/60) + (mydata1$Seconds_lon/3600))


dd <- data.frame(lat, lon)

write.xlsx(dd, file = "outputcoordinates.xlsx", sheetName = "conversion_mexico_Nizanda",  append=T)






### Decimal degrees minutes conversion

mydata2 <- read.xlsx("Coordinates individual plots meta analysis_R_inprogress.xlsx", 15)

df <- mydata2

df_lat <- data.frame(do.call('rbind', strsplit(as.character(df$lat),"[Â'' \"°NSâ€™â€™]",fixed=F)))
df_lat_Null <- df_lat[, colSums(df_lat != "") != 0]
names(df_lat_Null) <- c("Degrees_lat", "Minutes_lat", "Seconds_lat")

df_lon <- data.frame(do.call('rbind', strsplit(as.character(df$lon),"[Â'' \"°WEâ€™â€™]",fixed=F)))
df_lon_Null <- df_lon[, colSums(df_lon != "") != 0]
names(df_lon_Null) <- c("Degrees_lon", "Minutes_lon", "Seconds_lon")

mydataframe <- data.frame(df_lat_Null, df_lon_Null)

i <- sapply(mydataframe, is.factor)
mydataframe[i] <- lapply(mydataframe[i], as.character)
mydataframe[i] <- lapply(mydataframe[i], as.numeric)

lat <- (mydataframe$Degrees_lat + (mydataframe$Minutes_lat/60) + (mydataframe$Seconds_lat/3600))
lon <- (mydataframe$Degrees_lon + (mydataframe$Minutes_lon/60) + (mydataframe$Seconds_lon/3600))

dd <- data.frame(lat, lon)

write.xlsx(dd, file = "outputcoordinates.xlsx", sheetName = "conversion_Braz_Patos",  append=T)
