# Team DDJ
# Geo-Scripting Project Week 

# Plot Species Distribution Model for forest fragmentation 

# Modification date: 29-01-2015
# E-mail: jeroen.roelofs@.nl

#-------------------------------------- Function -----------------------------------
###################################### Settings ######################################

BufferRandomPointsInCountry <- function(Countrycode, AmountOfRandomPoints, BufferDistance) {
  
  NeedUTMOutput <- F #Set to True if used to convert it to UTM.
    
  ###################################### Main Script ######################################
  # Download administrative boundaries of the given country 
  CountryShape <- getData('GADM', country = Countrycode, level=1) ## administrative boundaries
  coordsys <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  
  # Transform country shape to projection type lat/lon
  spTransform(CountryShape, coordsys)
  
  # Set RandomPoints to NULL
  RandomPoints <- NULL
  StayInLoop <- TRUE
  
  while (StayInLoop) {## create random points
    dran <- runifpoint(AmountOfRandomPoints, win = as.vector(extent(CountryShape)))
    ## make the random point spatial points
    Spat <- SpatialPoints(data.frame(x = dran$x, y = dran$y), proj4string = CRS(proj4string(CountryShape)))
    ## select only the once within belgium
    RandomPoints <- gIntersection(Spat, CountryShape) # Comment if KML file is the input
  
    if (class(RandomPoints)[1] == "SpatialPoints") {
      StayInLoop <- FALSE
    }
  }
    
  # Pinpoint UTM location in UTM grid.
  X = RandomPoints@coords[1,1]
  Y = RandomPoints@coords[1,2]
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
  PointsUTM <- spTransform(RandomPoints, CRS(CRSText))
  
  # Buffers all the points.
  BufferUTM <- gBuffer(PointsUTM, width=BufferDistance)
  
  ### Sets projection back to WGS85 if TRUE.
  if (NeedUTMOutput == F) {
    BufferWGS <- spTransform(BufferUTM, CRS("+proj=longlat +datum=WGS84"))
    saveRDS(BufferWGS, file = "Data/BufferWGS.rds", ascii = FALSE, version = NULL,
            compress = TRUE, refhook = NULL)
  } else if (NeedUTMOutput == T) {
    saveRDS(BufferUTM, file = "Data/BufferUTM.rds", ascii = FALSE, version = NULL,
            compress = TRUE, refhook = NULL)
  }
  as = T
  return(as)
}
