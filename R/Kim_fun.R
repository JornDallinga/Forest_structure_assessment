Kim_fun <- function(Year, Data){
  # Get buffer locations
  
  buffer <- readRDS(file = "Data/BufferWGS.rds", refhook = NULL)
  pr <- getPR(buffer)
  pr_copy <- pr
  dir <- "data/" # If doesnt work add "./"
  extract <- 'extract_Kim/'
  
  # check for Path/Row existance
  if (is.list(pr_copy)) { # Assuming the list provided is the variable returned by getPR() function
    pr_copy <- pr_copy$PR
  }
  
  pr_copy <- sprintf('%06d', pr_copy) # Make the pr individual objects always 6 digits
  
  p <- substr(pr_copy,1,3)
  r <- substr(pr_copy,4,6)
  
  ## checking for patch/row existance
  start_list <- list()
  t <- 1
  for (i in 1:length(pr_copy)){
    urlP <- sprintf('LandsatFCC/WRS2/p%s/r%s/p%sr%s_FCC_%d', p[t], r[t], p[t], r[t], Year) #Path part of the url
    urlF <- sprintf('p%sr%s_FCC_%d_CM.tif.gz', p[t], r[t], Year) # Filename part of the url
    url <- sprintf('%s%s%s', baseURL = 'ftp://ftp.glcf.umd.edu/glcf/', urlP, urlF)
    Check <- url.exists(url = url)
    start_list[t] <- Check
    t <- t + 1
  }
  
  
  if ('FALSE' %in% start_list){
    return_list <- 'NA'
    print("Missing Path/Rows within selected buffer")
    
    return(return_list)
    
  } else {
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
    
    ## Copy Masked Raster for output figures
    Figure_output <- Masked_Raster
    
    if (Year == 19902000){
      # Set values and a value replacement function
      Masked_Raster[Masked_Raster < 5 | Masked_Raster > 20] <- 0
      Masked_Raster[Masked_Raster > 10 & Masked_Raster < 20] <- 1
      
      Figure_output[Figure_output > 20] <- 1 # non forest
      Figure_output[Figure_output == 2 | Figure_output == 3 | Figure_output == 0] <- 6 # No data or clouds
      Figure_output[Figure_output > 10 & Figure_output < 20] <- 2 # Forest
      Figure_output[Figure_output == 4] <- 3 # Water
      Figure_output[Figure_output == 6] <- 4 # No data or clouds
      
    } else if (Year == 20002005 & Data == 2000){
      
      Masked_Raster[Masked_Raster < 10 | Masked_Raster > 20] <- 0
      Masked_Raster[Masked_Raster > 10 & Masked_Raster < 20] <- 1
      
      Figure_output[Figure_output >= 20] <- 1
      Figure_output[Figure_output == 2 | Figure_output == 3 | Figure_output == 0] <- 6
      Figure_output[Figure_output > 10 & Figure_output < 20] <- 2
      Figure_output[Figure_output == 4] <- 3
      Figure_output[Figure_output == 6] <- 4
      
      
    } else if (Year == 20002005 & Data == 2005){
      Masked_Raster[Masked_Raster < 5 | Masked_Raster == 19 | Masked_Raster > 91] <- 0
      Masked_Raster[Masked_Raster == 11 | Masked_Raster == 91] <- 1
      
      Figure_output[Figure_output == 19 | Figure_output == 99] <- 1
      Figure_output[Figure_output == 2 | Figure_output == 3 | Figure_output == 0] <- 6
      Figure_output[Figure_output == 11 | Figure_output == 91] <- 2
      Figure_output[Figure_output == 4] <- 3
      Figure_output[Figure_output == 6] <- 4
      
      
    }
    
    names(Masked_Raster) <- "Kim"
    names(Figure_output) <- "Kim"
    
    return_list <- list(Masked_Raster, Water_perc, Clouds_perc, Figure_output)
    
    return (return_list)
  }
  
  
}  
