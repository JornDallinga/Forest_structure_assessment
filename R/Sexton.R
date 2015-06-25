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
  
  # list all files in folder
  list_file <- list.files(sprintf('%s/%s',dir,extract), full.names=FALSE)
  
  # Listing the pr_filenames in the list
  x <- listing_files(list_file, pr_filename)
  
  # Unpack VCF data
  Unpack_VCF(pr_filename, x, extract, Year, pr, dir)
  
  # list raster files
  list_file <- list.files(sprintf('%s/%s',dir,extract), full.names=FALSE)
  x_list <- listing_files(list_file, pr_filename)

  # Mosaicing if multiple data sets are listed, else it takes a single raster
  Masked_Raster <- Mosaic_Raster(x_list, dir, extract, buffer, pr_filename)
  
  # Set values and a value replacement function
  Masked_Raster[Masked_Raster < Threshold] <- 0
  Masked_Raster[Masked_Raster >= Threshold] <- 1
  Masked_Raster[Masked_Raster > 100] <- NA

  return (Masked_Raster)
  
}  

