plot_Figures <- function(Figure_output, j){
  
  dir.create(file.path(sprintf('output/Figures/Buffer_%s', BufferDistance)), showWarnings = FALSE)
  dir.create(file.path(sprintf('output/Figures/Buffer_%s/Threshold_%s', BufferDistance, Threshold)), showWarnings = FALSE)
  
  png(filename= sprintf("output/Figures/Buffer_%s/Threshold_%s/Forestcover%s_Year%s_Threshold%s_Buffer%s.png", BufferDistance, Threshold, j, Year, Threshold, BufferDistance))  
  
  k <- 1
  par(mfrow=c(2,2))
  
  ##########
  
  for (i in 1:length(Figure_output)){
    get_unique <- unique(Figure_output[[k]])
    
    within <- 1:4 %in% get_unique
    
    legend <- c("Non-forest", "forest", "water", "clouds/shadow")
    my_col = c('beige','darkgreen','blue', 'black')
    
    test <- matrix(, nrow= 4, ncol = 2)
    
    test[,1] <- legend
    test[,2] <- my_col
    
    # Create table of occuring catogories in Figure_output
    
    t <- 1
    for (i in 1:length(within)){
      if (within[t] == "FALSE") {
        test[t,] <- NA
        t <- (1 + t)
      } else if (within[t] == "TRUE") {
        t <- (1 + t)
      }
    }
    
    # remove NA's
    
    test <- test[rowSums(is.na(test)) == 0,]
    
    # preparing for plotting
    
    legend_list <- list(test[,1]) 
    legend <- unlist(legend_list)
    
    col_list <- list(test[,2])
    my_col <- unlist(col_list)
    
    plot(Figure_output[[k]], legend = F, col = my_col, main = names(Figure_output[[k]]))
    legend(x='topleft', legend = legend, fill = my_col)
    
    k <- k + 1
  } 
  dev.off()
  
}



