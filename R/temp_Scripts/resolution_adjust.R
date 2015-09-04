## bug in data when resolution between raster differs

reso_adjust <- function(new_list){
  t <- 1
  test <- matrix(, nrow = length(new_list), ncol = 2)
  
  for (i in new_list){
    test[t,] <- c(xres(new_list[[t]]), yres(new_list[[t]]))
    t <- t + 1
  }
  
  temp <- table(test)
  output <- names(temp)[temp == max(temp)]

  }
  
  return(output)
  
}

 