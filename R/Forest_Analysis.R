Forest_Analysis <- function(Year){
  if (Year == 1990){
    K_1990 <- Kim_1990(Year = 19902000)
    SDMK_1990 <- SDM_function(K_1990)
    SDMK_col <- ncol(SDMK_1990) + 6
    mat3[i, 7:SDMK_col] <<- SDMK_col
    
    if (j < 1) {
      colnames(SDMK_1990) -> K_1990_colnames
      names(mat3) <<- c("Country", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", K_1990_colnames)
    } else{
      
    } 
    
    
  } else if (Year == 2000){
    S <- Sexton(Year, Threshold)
    SDMS_2000 <- SDM_function(S)
    SDMS_col <- ncol(SDMS_2000) + 6
    mat[i, 7:SDMS_col] <<- SDMS_2000
    
    H <- Hansen(Threshold, Year)
    SDMH <- SDM_function(H)
    SDMH_col <- ncol(SDMH) + 6
    mat1[i, 7:SDMH_col] <<- SDMH
    
    K_2000 <- Kim_2000(Year = 20002005)
    SDMK_2000 <- SDM_function(K_2000)
    SDMK_col <- ncol(SDMK_2000) + 6
    mat2[i, 7:SDMK_col] <<- SDMK_2000
    
    if (j < 1) {
      colnames(SDMS_2000) -> SDMS_2000_colnames
      names(mat) <<- c("Country", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", SDMS_2000_colnames)
      colnames(SDMK_2000) -> K_2000_colnames
      names(mat2) <<- c("Country", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", K_2000_colnames)
      colnames(SDMH) -> SDMH_colnames
      names(mat1) <<- c("Country", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", SDMH_colnames)
    } else{
      
    } 
    
    
  } else if (Year == 2005){
    S <- Sexton(Year, Threshold)
    SDMS_2005 <- SDM_function(S)
    SDMS_col <- ncol(SDMS_2005) + 6
    mat4[i, 7:SDMS_col] <<- SDMS_2005
    
    K_2005 <- Kim_2005(Year = 20002005)
    SDMK_2005 <- SDM_function(K_2005)
    SDMK_col <- ncol(SDMK_2005) + 6
    mat5[i, 7:SDMK_col] <<- SDMK_2005
    
    if (j < 1) {
      colnames(SDMS_2005) -> SDMS_2005_colnames
      names(mat4) <<- c("Country", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", SDMS_2005_colnames)
      colnames(SDMK_2005) -> K_2005_colnames
      names(mat5) <<- c("Country", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", K_2005_colnames)
    } else{
      
    } 
    
    
  } else if (Year == 2012) {
    H <- Hansen(Threshold, Year)
    SDMH_2012 <- SDM_function(H)
    SDMH_col <- ncol(SDMH_2012) + 6
    mat6[i, 7:SDMH_col] <<- SDMH_2012
    if (j < 1) {
      colnames(SDMH_2012) -> SDMH_2012_colnames
      names(mat6) <<- c("Country", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", SDMH_2012_colnames)
    } else{
      
    } 
  
    
  } else {
    warning("No valid year")
  }
  
  
}