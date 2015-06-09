

Kim_2000 <- function(Year){
  # Get buffer locations
  
  buffer <- readRDS(file = "Data/BufferWGS.rds", refhook = NULL)
  pr <- getPR(buffer)
  dir <- "data/" # If doesnt work add "./"
  
  # Download data
  downloadPR(pr, Year, dir, log = NULL, baseURL = "ftp://ftp.glcf.umd.edu/glcf/")
  
  # Create names for unpacking
  p_filename <- sprintf("%03d", pr$PATH)
  r_filename <- sprintf("%03d", pr$ROW)
  pr_filename <- sprintf("p%sr%s_FCC_%s_CM.tif", p_filename, r_filename, Year)
  
  # create list of extraction map
  x <- list.files(sprintf('%s/%s',dir,'extract_kim/'), full.names=FALSE)

  # Unpack VCF data
  if (pr_filename %in% x){
    print("File allready exsist, no need for unpacking")
  } else {
    unpackVCF(pr=pr, year = Year, searchDir=dir, dir=sprintf('%s/%s',dir,'extract_kim/'))
    
  }
  
  # Load data into R environment
  Raster <- raster(sprintf("data/extract_kim/%s" , pr_filename))
  
  # Masking the Raster to the buffer area
  transform_buffer<- spTransform(buffer, CRS(proj4string(Raster))) 
  Masked_Raster <- mask(Raster, transform_buffer)
  
  # Cropping masked extent
  Buffer_Crop <- crop(Masked_Raster, transform_buffer)
  
  # Set values and a value replacement function
  Buffer_Crop[Buffer_Crop < 5 | Buffer_Crop > 20] <- 0
  Buffer_Crop[Buffer_Crop > 10 & Buffer_Crop < 20] <- 1
  
  return (Buffer_Crop)
  
}  
