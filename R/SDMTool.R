# Daniel Scheerooren
# Geo-Scripting Project Week 

# Species Distribution Model for forest fragmentation 

# Modification date: 29-01-2015
# E-mail: daniel.scheerooren@wur.nl

#-------------------------------------- Function -----------------------------------

SDM_function <- function(S){
  asc.from.raster(S)
  head(S)
  
  ## Label the connected components (create islands with same labels when cells are connected)
  ccl.mat <- ConnCompLabel(S)
  
  # Calculate the patch statistics
  ps.data <- PatchStat(ccl.mat)
  ps.data

  nr_patches <-count(ps.data$patchID)
  nr_patches <- sum(nr_patches$freq)
  return (nr_patches)
}

