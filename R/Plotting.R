Plot_function <- function(Year, BufferDistance, Threshold, Plot_Raster){
  
  dir.create(file.path('output/Figures'), showWarnings = FALSE)
  
  png(filename= sprintf("output/Figures/Forestcover%s_Year%s_Threshold%s_Buffer%s.png",j, Year, Threshold, BufferDistance))  
  t <-  1
  par(mfrow=c(2,2))
  
  for (i in 1:length(Plot_Raster)){
    
    plot(stack(Plot_Raster[t]), main = names(stack(Plot_Raster[t])))
    t <- 1 + t
    
  }
  
  dev.off()
  
}

