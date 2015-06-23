
Forest_cover <- function(raster){
  
  ## caluclating Forest and Non forest cells
  Forest <- freq(raster, digits= 0, value = 1, useNA = no)
  Non_Forest <- freq(raster, digits= 0, value = 0, useNA = no)
  
  ## Counting the total number of cells, also including other cells such as clouds, but excluding NA
  listvalues <- values(raster)
  countcells <- count(listvalues, 1)
  countcells <- countcells[!is.na(countcells$x),]
  total_cells <- sum(countcells$freq)
  
  ## percentage forest cover
  forest_perc <- (Forest / total_cells) * 100
  
  # list <- c(Forest, Non_Forest)

  return(forest_perc)
}