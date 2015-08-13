Kim_fun <- function(Year){
  # Get buffer locations
  
  buffer <- readRDS(file = "Data/BufferWGS.rds", refhook = NULL)
  pr <- getPR(buffer)
  dir <- "data/" # If doesnt work add "./"
  extract <- 'extract_Kim/'


  # Download data
  downloadPR(pr, Year, dir, log = NULL, baseURL = "ftp://ftp.glcf.umd.edu/glcf/")
  
  # Create names for unpacking
  p_filename <- sprintf("%03d", pr$PATH)
  r_filename <- sprintf("%03d", pr$ROW)
  pr_filename <- sprintf("p%sr%s_FCC_%s_CM.tif", p_filename, r_filename, Year)
  
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
  
  # retrieve water
  Water <- freq(Masked_Raster, digits= 0, value = 4, useNA = no)
  listvalues <- values(Masked_Raster)
  countcells <- count(listvalues)
  countcells <- countcells[!is.na(countcells$x),]
  total_cells <- sum(countcells$freq)
  ## percentage water
  Water_perc <- (Water / total_cells) * 100
  
  # retrieve clouds
  Clouds <- freq(Masked_Raster, digits= 0, value = 2 | 3, useNA = no)
  listvalues <- values(Masked_Raster)
  countcells <- count(listvalues)
  countcells <- countcells[!is.na(countcells$x),]
  total_cells <- sum(countcells$freq)
  ## percentage water
  Clouds_perc <- (Clouds / total_cells) * 100
  
  
  if (Year == 19902000){
    # Set values and a value replacement function
    Masked_Raster[Masked_Raster < 5 | Masked_Raster > 20] <- 0
    Masked_Raster[Masked_Raster > 10 & Masked_Raster < 20] <- 1
  } else if (Year == 20002005){
    Masked_Raster[Masked_Raster < 10 | Masked_Raster >= 20] <- 0
    Masked_Raster[Masked_Raster > 10 & Masked_Raster < 20] <- 1
  } else if (Year == 20002005){
    Masked_Raster[Masked_Raster < 5 | Masked_Raster == 19 | Masked_Raster > 91] <- 0
    Masked_Raster[Masked_Raster == 11 | Masked_Raster == 91] <- 1
  }

  names(Masked_Raster) <- "Kim"
  
  return_list <- list(Masked_Raster, Water_perc, Clouds_perc)
  
  return (return_list)
  
}  
