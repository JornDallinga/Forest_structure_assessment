Forest_Analysis <- function(Year = Year, Countrycode = Countrycode, Chronosequence = Chronosequence, BufferDistance = BufferDistance, Threshold = Threshold){
  
  caption <- c("Country","Chronosequence", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", "Forest_cover")
  
  if (Year == 1990){
    ## writing metadata to matrix
    mat3 <- Write_metadata(mat = mat3 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    
    ## calculating forest cover raster
    K_1990 <- Kim_1990(Year = 19902000)
    
    ## applying SDM function to forest cover raster
    SDMK_1990 <- SDM_function(K_1990)
    
    ## reading number of columns from SDM function output
    SDMK_col <- ncol(SDMK_1990) + 8
    
    ## writing results to matrix
    mat3[i, 9:SDMK_col] <- SDMK_1990
    
    ## Forest cover calc
    FC_K_1990 <- Forest_cover(K_1990)
    mat3[i, 8] <- FC_K_1990
    
    ## assigning col names
    if (j < 1) {
      colnames(SDMK_1990) -> K_1990_colnames
      names(mat3) <- c(caption, K_1990_colnames)
    } else{
      
    } 
    
    ## create global variable of matrix
    mat3 <<- mat3
    
    matrix_list <- list(mat3 = mat3)

    
    ## listing raster files and creating forest cover figure
    Plot_Raster <- list(K_1990)
    Plot_function(Year = Year, BufferDistance = BufferDistance, Threshold = Threshold, Plot_Raster)
    
  } else if (Year == 2000){
    ## writing metadata to matrix
    mat <- Write_metadata(mat = mat ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    
    ## calculating forest cover raster
    S <- Sexton(Year, Threshold)
    
    ## applying SDM function to forest cover raster
    SDMS_2000 <- SDM_function(S)
    
    ## reading number of columns from SDM function output
    SDMS_col <- ncol(SDMS_2000) + 8
    
    ## writing results to matrix
    mat[i, 9:SDMS_col] <- SDMS_2000
    
    ## Forest cover calc
    FC_S_2000 <- Forest_cover(S)
    mat[i, 8] <- FC_S_2000
    
    
    ## writing metadata to matrix
    mat1 <- Write_metadata(mat = mat1 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    
    ## calculating forest cover raster
    H <- Hansen(Threshold, Year)
    
    ## applying SDM function to forest cover raster
    SDMH <- SDM_function(H)
    
    ## reading number of columns from SDM function output
    SDMH_col <- ncol(SDMH) + 8
    
    ## writing results to matrix
    mat1[i, 9:SDMH_col] <- SDMH
    
    ## Forest cover calc
    FC_H_2000 <- Forest_cover(H)
    mat1[i, 8] <- FC_H_2000
    
    
    ## writing metadata to matrix
    mat2 <- Write_metadata(mat = mat2 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    
    ## calculating forest cover raster
    K_2000 <- Kim_2000(Year = 20002005)
    
    ## applying SDM function to forest cover raster
    SDMK_2000 <- SDM_function(K_2000)
    
    ## reading number of columns from SDM function output
    SDMK_col <- ncol(SDMK_2000) + 8
    
    ## writing results to matrix
    mat2[i, 9:SDMK_col] <- SDMK_2000
    
    ## Forest cover calc
    FC_K_2000 <- Forest_cover(K_2000)
    mat2[i, 8] <- FC_K_2000
    
    ## assigning col names
    if (j < 1) {
      colnames(SDMS_2000) -> SDMS_2000_colnames
      names(mat) <- c(caption, SDMS_2000_colnames)
      colnames(SDMK_2000) -> K_2000_colnames
      names(mat2) <- c(caption, K_2000_colnames)
      colnames(SDMH) -> SDMH_colnames
      names(mat1) <- c(caption, SDMH_colnames)
    } else{
      
    } 
    
    ## create global variable of matrix
    mat <<- mat
    mat1 <<- mat1
    mat2 <<- mat2
    
    matrix_list <- list(mat = mat, mat1 = mat1, mat2 = mat2)

    
    ## listing raster files and creating forest cover figure
    Plot_Raster <- list(S, H, K_2000)
    Plot_function(Year = Year, BufferDistance = BufferDistance, Threshold = Threshold, Plot_Raster)
    
    
    
  } else if (Year == 2005){
    ## writing metadata to matrix
    mat4 <- Write_metadata(mat = mat4 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    
    ## calculating forest cover raster
    S <- Sexton(Year, Threshold)
    
    ## applying SDM function to forest cover raster
    SDMS_2005 <- SDM_function(S)
    
    ## reading number of columns from SDM function output
    SDMS_col <- ncol(SDMS_2005) + 8
    
    ## writing results to matrix
    mat4[i, 9:SDMS_col] <- SDMS_2005
    
    ## Forest cover calc
    FC_S_2005 <- Forest_cover(S)
    mat4[i, 8] <- FC_S_2005

    
    ## writing metadata to matrix
    mat5 <- Write_metadata(mat = mat5 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    
    ## calculating forest cover raster
    K_2005 <- Kim_2005(Year = 20002005)
    
    ## applying SDM function to forest cover raster
    SDMK_2005 <- SDM_function(K_2005)
    
    ## reading number of columns from SDM function output
    SDMK_col <- ncol(SDMK_2005) + 8
    
    ## writing results to matrix
    mat5[i, 9:SDMK_col] <- SDMK_2005
    
    ## Forest cover calc
    FC_K_2005 <- Forest_cover(K_2005)
    mat5[i, 8] <- FC_K_2005
    
    ## assigning col names
    if (j < 1) {
      colnames(SDMS_2005) -> SDMS_2005_colnames
      names(mat4) <- c(caption, SDMS_2005_colnames)
      colnames(SDMK_2005) -> K_2005_colnames
      names(mat5) <- c(caption, K_2005_colnames)
    } else {
      
    } 
    
    ## create global variable of matrix
    mat4 <<- mat4
    mat5 <<- mat5
    
    matrix_list <- list(mat = mat4, mat = mat5)
    
    ## listing raster files and creating forest cover figure
    Plot_Raster <- list(S, K_2005)
    Plot_function(Year = Year, BufferDistance = BufferDistance, Threshold = Threshold, Plot_Raster)
    
  } else if (Year == 2012) {
    
    ## writing metadata to matrix
    mat6 <- Write_metadata(mat = mat6 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    
    ## calculating forest cover raster
    H <- Hansen(Threshold, Year)
    
    ## applying SDM function to forest cover raster
    SDMH_2012 <- SDM_function(H)
    
    ## reading number of columns from SDM function output
    SDMH_col <- ncol(SDMH_2012) + 8
    
    ## writing results to matrix
    mat6[i, 9:SDMH_col] <- SDMH_2012
    
    ## Forest cover calc
    FC_H_2012 <- Forest_cover(H)
    mat6[i, 8] <- FC_H_2012
    
    ## assigning col names
    if (j < 1) {
      colnames(SDMH_2012) -> SDMH_2012_colnames
      names(mat6) <- c(caption, SDMH_2012_colnames)
      
    } else {
      
    } 
    
    ## create global variable of matrix
    mat6 <<- mat6
    
    matrix_list <- list(mat = mat6)
    
    
    ## listing raster files and creating forest cover figure
    Plot_Raster <- list(H)
    Plot_function(Year = Year, BufferDistance = BufferDistance, Threshold = Threshold, Plot_Raster)
    
  } else {
    warning("No valid year")
  }
  
  return(matrix_list)


}