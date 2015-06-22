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
  
  # Set mask over tresholded gfc, size of buffer

  if (year == 2000){
    ## create forest cover mask year 2000
    mask_gfc <- mask(gfc_thresholded$forest2000, aoi)
  } else if (year == 2012){
    ## create forest cover loss map 2012
    mask_gfc_loss <- mask(gfc_thresholded$lossyear, aoi)
    #### Set values and a value replacement function
    mask_gfc_loss[mask_gfc_loss > 0 & mask_gfc_loss < 13]  <- 1
    
    ## create forest cover gain map 2012
    mask_gfc_gain <- mask(gfc_thresholded$gain, aoi)
    
    ## create forest cover lossgain 2012
    mask_gfc_lossgain <- mask(gfc_thresholded$lossgain, aoi)
    
    ## Create forest cover map 2012
    mask_gfc <- mask(gfc_thresholded$forest2000, aoi)
    mask_gfc <- (((mask_gfc - mask_gfc_loss) + mask_gfc_gain) + mask_gfc_lossgain)
    
  } else {
    warning("invalid year")
  }

  
  # test_Hansen <- writeRaster(mask_gfc, filename = "output/hansen.tif", overwrite = T)
  # kml(test_Hansen, colour = "GREEN")
  
  return (mask_gfc)
}
