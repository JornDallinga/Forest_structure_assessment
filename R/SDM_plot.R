# Team DDJ
# Geo-Scripting Project Week 

# Plot Species Distribution Model for forest fragmentation 

# Modification date: 29-01-2015
# E-mail: daniel.scheerooren@wur.nl

#-------------------------------------- Function -----------------------------------
SDM_plot <- function(S){
  asc.from.raster(S)
  head(S)
  
  ## Label the connected components (create islands with same labels when cells are connected)
  ccl.mat <- ConnCompLabel(S)
  
  ## Visualize raster with fragmentation
  # plot(ccl.mat, main = paste("Fragmentation in", Countrycode, "of a plot, in the year", Year, "with a threshold of", Threshold))
  return(ccl.mat) 
}