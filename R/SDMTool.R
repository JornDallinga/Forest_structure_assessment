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
  
  ##### testing data frame
  
  
  ####
  c1.data <- ClassStat(S)
  
  if (c1.data[1,1] == 0){
    c1.data <- c1.data[-1,]
  } else {
    
  }
  
  ##### testing data frame
  
  nr_patches <-count(ps.data$patchID)
  nr_patches <- sum(nr_patches$freq)
  return (c1.data)
}

c1.data
nr_patches
nr_patches <- (ccl.mat$patchID)


c1.data
c1.data[1:2]

write.xlsx(c1.data, sprintf("output/test.xlsx"), sheetName = , append = TRUE)
write.xlsx(c1.data, sprintf("output/test.xlsx"), col.names = F, row.names = F)