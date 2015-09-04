plot_Figures <- function(Figure_output, j){
  
  dir.create(file.path('output/Figures'), showWarnings = FALSE)
  dir.create(file.path(sprintf('output/Figures/%s', Chronosequence)), showWarnings = FALSE)
  dir.create(file.path(sprintf('output/Figures/%s/Buffer_%s', Chronosequence, BufferDistance)), showWarnings = FALSE)
  dir.create(file.path(sprintf('output/Figures/%s/Buffer_%s/Threshold_%s', Chronosequence, BufferDistance, Threshold)), showWarnings = FALSE)
  
  png(filename= sprintf("output/Figures/%s/Buffer_%s/Threshold_%s/PlotID_%s_Year%s_Threshold%s_Buffer%s.png", Chronosequence, BufferDistance, Threshold, Plot_ID, Year, Threshold, BufferDistance), width = 1200, height = 800)  
  
  k <- 1
  par(mfrow=c(2,2), mar=c(3, 3, 3, 15), cex = 1.1)
  
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
    
    if (class(test) == "matrix"){
      
      legend_list <- list(test[,1]) 
      legend <- unlist(legend_list)
      
      col_list <- list(test[,2])
      my_col <- unlist(col_list)
      
    } else {
      
      legend_list <- list(test[1]) 
      legend <- unlist(legend_list)
      
      col_list <- list(test[2])
      my_col <- unlist(col_list)
    }

    
    plot(Figure_output[[k]], legend = F, col = my_col, main = sprintf("%s, %s, PlotID: %s", names(Figure_output[[k]]), Year, Plot_ID))
    par(xpd = TRUE, cex = 1.1)
    # Create y axes point for placing legend
    set_y <- (ymin(Figure_output[[k]]) + ymax(Figure_output[[k]])) / 2
    
    # Using usr to get coordinate extreme for x axis to place the legend
    legend(par()$usr[2], set_y, legend = legend, fill = my_col)
    
    k <- k + 1
  } 
  dev.off()
  
}



