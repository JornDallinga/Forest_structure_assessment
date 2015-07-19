shape <- readOGR(dsn = "D:/R_Projects/transects", layer = "transects" )

trueCentroids = gCentroid(shape,byid=TRUE)
plot(shape)
points(coordinates(shape),pch=1)
points(trueCentroids,pch=2)

data_coords <- data.frame(shape)

data_coords[2] <- trueCentroids@coords[,"x"]
data_coords[3] <- trueCentroids@coords[,"y"]

Coords_WGS <- spTransform(trueCentroids, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


x_lat <- Coords_WGS@coords[,"x"]
y_lon <- Coords_WGS@coords[,"y"]

data_coords[4] <- x_lat 
data_coords[5] <- y_lat 

colnames(data_coords) <- c("Transects", "x_UTM", "y_UTM", "x_WGS", "y_WGS")

write.xlsx(data_coords, file = "outputcoordinates.xlsx", sheetName = "conversion_Panama_Barro_Colorado_Island",  append=T)
