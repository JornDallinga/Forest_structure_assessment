# Team DDJ
# Geo-Scripting Project Week 

# Global Forest Cover Analysis with Hansen data

# Modification date: 29-01-2015
# E-mail: daniel.scheerooren@wur.nl

#-------------------------------------- Function -----------------------------------

Hansen <- function(Threshold, year = Year){
  ## Create variable Area Of Interest (aio)
  aoi <- readRDS(file = 'data/BufferWGS.rds', refhook = NULL)
    
  ## Calculate tiles needed to cover the AOI
  tiles <- calc_gfc_tiles(aoi)
  print(length(tiles))
   
  ## Download GFC data
  download_tiles(tiles, 'data', first_and_last=FALSE)
  
  ## Extract data from tiles
  gfc_extract <- extract_gfc(aoi, "data", filename="data/extract_hansen/GFC_extract.tif", overwrite=TRUE)
  
  
  ## Apply threshold to extracted data 
  gfc_thresholded <- threshold_gfc(gfc_extract, Threshold=Threshold, 
                                   filename="data/extract_hansen/GFC_extract_thresholded.tif", overwrite=TRUE)
  
  
  # retrieve water
  Water <- freq(gfc_thresholded$datamask, digits= 0, value = 2, useNA = no)
  listvalues <- values(gfc_thresholded$datamask)
  countcells <- count(listvalues)
  countcells <- countcells[!is.na(countcells$x),]
  total_cells <- sum(countcells$freq)
  ## percentage water
  Water_perc <- (Water / total_cells) * 100
  
  ## Masking gfc data to aoi
  mask_gfc <- mask(gfc_thresholded, aoi)
  
  
  ## creating figure output
  Figure_output <- mask_gfc
  
  Figure_output$forest2000[Figure_output$forest2000 == 1] <- 2 # Forest
  Figure_output$forest2000[Figure_output$forest2000 == 0] <- 1 # Non-Forest
  Figure_output$datamask[Figure_output$datamask == 2] <- 3 # water
  Figure_output$datamask[Figure_output$datamask == 0] <- 4 # No data
  suppressWarnings(Figure_output$datamask[Figure_output$datamask < 3] <- NA) # Nodata for merging

  
  Figure_output <- merge(Figure_output$forest2000, Figure_output$datamask, overlap = T)
  
  names(Figure_output) <- "Hansen"
  
  
  # Set mask over tresholded gfc, size of buffer

  if (year == 2000){
    ## create forest cover mask year 2000
    mask_gfc <- mask_gfc$forest2000
    
  } else if (year == 2012){
    ## create forest cover loss map 2012
    mask_gfc_loss <- mask_gfc$lossyear
    #### Set values and a value replacement function
    mask_gfc_loss[mask_gfc_loss > 0 & mask_gfc_loss < 13]  <- 1
    
    ## create forest cover gain map 2012
    mask_gfc_gain <- mask_gfc$gain
    
    ## create forest cover lossgain 2012
    mask_gfc_lossgain <- mask_gfc$lossgain
    
    ## Create forest cover map 2012
    mask_gfc <- mask_gfc$forest2000
    mask_gfc <- (((mask_gfc - mask_gfc_loss) + mask_gfc_gain) + mask_gfc_lossgain)
    
  } else {
    warning("invalid year")
  }

  
  names(mask_gfc) <- "Hansen"

  
  return_list <- list(mask_gfc, Water_perc, Figure_output)
  
  return (return_list)
}
