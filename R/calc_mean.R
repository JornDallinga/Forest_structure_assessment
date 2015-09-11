calc_mean <- function(mat){
  unique_test <- unique(mat$Chronosequence)
  mean_matrix <- matrix(, nrow = length(unique_test), ncol = 43)
  #norm_matrix <- matrix(, nrow = length(unique_test), ncol = 43)
  mean_matrix <- data.frame(mean_matrix)
  colnames(mean_matrix)[1:2] <- names(mat)[1:2]
  suppressWarnings(colnames(mean_matrix)[3:43] <- names(mat)[9:50])

  t <- 1
  
  for (i in 1:length(unique_test)){
    select_chrono <- subset(mat, grepl(unique_test[t], Chronosequence, fixed=TRUE))

    #scaled.dat <- select_chrono
    #scaled.dat[9:49] <- scale(select_chrono[9:49], center = T, scale = apply(select_chrono[9:49], 2, sd, na.rm = TRUE))
    #scaled.dat[9:49] <- scale(select_chrono[9:49])
    
    #colMeans(scaled.dat[9:49])  # faster version of apply(scaled.dat, 2, mean)
    #scaled.dat[9:49] <- apply(scaled.dat[9:49], 2, sd)
    
    # mean_matrix[t,1:2] <- c(unique(select_chrono$Country), unique(select_chrono$Chronosequence))
    
    mean_matrix[t,1:2] <- c(unique(select_chrono$Country), unique(select_chrono$Chronosequence))
    mean_matrix[t,3:length(mean_matrix)]<- colMeans(select_chrono[9:length(select_chrono)])
    
    
    t <- t + 1
  }

  
  
  return (mean_matrix)
}


