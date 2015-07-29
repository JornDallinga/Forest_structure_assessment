calc_mean <- function(mat){
  unique_test <- unique(mat$Chronosequence)
  mean_matrix <- matrix(, nrow = length(unique_test), ncol = 3)
  colnames(mean_matrix) <- c("Country", "Chronosequence", "Forest_cover")
  t <- 1
  
  for (i in 1:length(unique_test)){
    select_chrono <- subset(mat, grepl(unique_test[t], Chronosequence, fixed=TRUE))
    
    mean_matrix[t,1] <- unique(select_chrono$Country)
    mean_matrix[t,2] <- unique(select_chrono$Chronosequence)
    mean_matrix[t,3] <- mean(select_chrono$Forest_cover)
    t <- t + 1
  }
  mat_year <- unique(mat$Year)
  mat_buffer <- unique(mat$Buffer)
  mat_threshold <- unique(mat$Threshold)
  

  #sheetnam <- substitute(mat)
  #sheetnam <- toString(sheetnam)
  
  return (mean_matrix)
}

