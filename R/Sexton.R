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
  extract <- 'extract_sexton/'
  
  # Download data
  
  downloadPR(pr, Year, dir, log = NULL,
             baseURL = "ftp://ftp.glcf.umd.edu/glcf/")
    
  
  # Create names for unpacking
  p_filename <- sprintf("%03d", pr$PATH)
  r_filename <- sprintf("%03d", pr$ROW)
  pr_filename <- sprintf("p%sr%s_TC_%s.tif", p_filename, r_filename, Year)
  
  # create list of extraction map
  x <- list.files(sprintf('%s/%s',dir,extract), full.names=FALSE)
  
  # Unpack VCF data
  Unpack_VCF(pr_filename, x, extract, Year, pr, dir)
  
  # Load data into R environment
  Raster <- raster(sprintf("%s%s%s", dir, extract, pr_filename))
  
  # Cropping masked extent
  transform_buffer<- spTransform(buffer, CRS(proj4string(Raster))) 
  Crop_Raster <- crop(Raster, transform_buffer)
  
  # Masking the Raster to the buffer area
  Masked_Raster <- mask(Crop_Raster, transform_buffer, progress = "window")
  
  # Set values and a value replacement function
  Masked_Raster[Masked_Raster < Threshold] <- 0
  Masked_Raster[Masked_Raster >= Threshold] <- 1
  Masked_Raster[Masked_Raster > 100] <- NA
  
  
  #Mask_proj <- projectRaster(Masked_Raster, crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
#   test_Sexton <- writeRaster(Mask_proj, filename = "output/sexton.tif", overwrite = TRUE)
#   kml(test_Sexton, colour = "RED")
  
  return (Masked_Raster)
  
}  

