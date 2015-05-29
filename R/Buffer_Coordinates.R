Buffertest <- function(Countrycode, BufferDistance) {
  
  NeedUTMOutput <- F #Set to True if used to convert it to UTM.
  
  ###################################### Main Script ######################################
  # Download administrative boundaries of the given country 
  CountryShape <- getData('GADM', country = Countrycode, level=1) ## administrative boundaries
  coordsys <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  
  # Transform country shape to projection type lat/lon
  spTransform(CountryShape, coordsys)
  
  #dran$x = coordinates
  #dran$y = coordinates
  
  ## make the random point spatial points
  Spat <- SpatialPoints(data.frame(x = 9.703981, y = -83.629092), proj4string = CRS(proj4string(CountryShape)))
  
}

############################################################
Buffer_Point <- function(Countrycode, BufferDistance) {
  
  
  NeedUTMOutput <- F #Set to True if used to convert it to UTM.
  
  CountryShape <- getData('GADM', country = 'CRI', level=1)
  coordsys <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  spTransform(CountryShape, coordsys)
  Spat <- SpatialPoints(data.frame(x = mydata$x[2], y = mydata$y[2]), proj4string = CRS(proj4string(CountryShape)))
  Spat
  
  # Pinpoint UTM location in UTM grid.
  X = Spat@coords[1,1]
  Y = Spat@coords[1,2]
  UTMLocation <- utm_zone(x = X, y = Y)
  
  # Make location ready for projection change.
  Zone <- substr(UTMLocation, 1, 2)
  Hemisphere <- substr(UTMLocation, 3,3)
  
  # Hemisphere <- "N"
  # Hemisphere <- "S"
  
  # Assign String values for CRS input.
  if(Hemisphere == "N") {
    HemiText <- "+north"
  } else if (Hemisphere == "S") {
    HemiText <- "+south"
  } else stop("Not a correct Hemisphere given")
  ZoneText = paste("+zone=", Zone, sep = "")
  
  # Combine prepared strings for final input string.
  CRSText <- paste("+proj=utm", ZoneText, HemiText, sep = " ")
  
  # Transform WGS to UTM.
  PointsUTM <- spTransform(Spat, CRS(CRSText))
  
  # Buffers all the points.
  BufferUTM <- gBuffer(PointsUTM, width=BufferDistance)
  
  if (NeedUTMOutput == F) {
    BufferWGS <- spTransform(BufferUTM, CRS("+proj=longlat +datum=WGS84"))
    saveRDS(BufferWGS, file = "Data/BufferWGS.rds", ascii = FALSE, version = NULL,
            compress = TRUE, refhook = NULL)
  } else if (NeedUTMOutput == T) {
    saveRDS(BufferUTM, file = "Data/BufferUTM.rds", ascii = FALSE, version = NULL,
            compress = TRUE, refhook = NULL)
  }
  
  #as = T
  #return(as)
  
  plot(CountryShape)
  plot(BufferWGS, add=TRUE, col = "red")
  
}


Spat
CountryShape
