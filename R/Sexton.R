# Team DDJ
# Geo-Scripting Project Week 

# Plot Species Distribution Model for forest fragmentation 

# Modification date: 29-01-2015
# E-mail: jorn.dallinga@wur.nl

#-------------------------------------- Function -----------------------------------

Sexton <- function(Year, Threshold){
  # Get buffer locations
  
  buffer <- readRDS(file = "Data/BufferWGS.rds", refhook = NULL)
  pr <- getPR(buffer)
  dir <- "data/" # If doesnt work add "./"
  
  # Download data
  downloadPR(pr, Year, dir, log = NULL,
             baseURL = "ftp://ftp.glcf.umd.edu/glcf/LandsatTreecover/WRS2/")
    
  # Unpack VCF data
  unpackVCF(pr=pr, year = Year, searchDir=dir, dir=sprintf('%s/%s',dir,'extract_sexton/'))
  x <- list.files(sprintf('%s/%s',dir,'extract_sexton/'), full.names=TRUE)
  
  # Load data into R environment
  Raster <- raster(x[1])
  
  # Masking the Raster to the buffer area
  transform_buffer<- spTransform(buffer, CRS(proj4string(Raster))) 
  Masked_Raster <- mask(Raster, transform_buffer)
  
  # Cropping masked extent
  Buffer_Crop <- crop(Masked_Raster, transform_buffer)
  
  # Set values and a value replacement function
  Buffer_Crop[Buffer_Crop < Threshold] <- 0
  Buffer_Crop[Buffer_Crop >= Threshold] <- 1
  Buffer_Crop[Buffer_Crop > 100] <- NA
  
  # Clear directory to prevent extraction errors
  unlink("data/extract_sexton/*.tif", recursive = FALSE)
  
  return (Buffer_Crop)
  
}  

