

###
# Lat = Y Long = X
mydata <- read.xlsx("Coordinates individual plots meta analysis_R_inprogress.xlsx", 4)

d <- data.frame(x = mydata$Longitude, y = mydata$Latitude)
point <- SpatialPoints(data.frame(x = d$x, y = d$y))
proj4string(point) <- CRS("+proj=utm +zone=19 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
test <- spTransform(point, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
coords_output <-data.frame(coordinates(test))

d[3] <- coords_output[1]
d[4] <- coords_output[2]

write.xlsx(d, file = "outputcoordinates.xlsx", sheetName = "conversion_mex",  append=T)
