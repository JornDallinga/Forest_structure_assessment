calc_mean <- function(mat){
  unique_test <- unique(mat$Chronosequence)
  mean_matrix <- matrix(, nrow = length(unique_test), ncol = 8)
  colnames(mean_matrix) <- c("Country", "Chronosequence", "Forest_cover", "Water_cover", "Cloud_cover", "n_patches", "Total_edge", "Patch_cohesion")
  t <- 1
  
  for (i in 1:length(unique_test)){
    select_chrono <- subset(mat, grepl(unique_test[t], Chronosequence, fixed=TRUE))
    
    mean_matrix[t,1] <- unique(select_chrono$Country)
    mean_matrix[t,2] <- unique(select_chrono$Chronosequence)
    mean_matrix[t,3] <- mean(select_chrono$Forest_cover)
    mean_matrix[t,4] <- mean(select_chrono$Water_cover)
    mean_matrix[t,5] <- mean(select_chrono$Cloud_cover)
    mean_matrix[t,6] <- mean(select_chrono$n.patches)
    mean_matrix[t,7] <- mean(select_chrono$total.edge)
    mean_matrix[t,8] <- mean(select_chrono$patch.cohesion.index)
    t <- t + 1
  }
  

  return (mean_matrix)
}

