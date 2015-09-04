Forest_Analysis <- function(Year = Year, Countrycode = Countrycode, Plot_ID = Plot_ID, Chronosequence = Chronosequence, BufferDistance = BufferDistance, Threshold = Threshold){
  
  caption <- c("Country","Chronosequence","Plot_ID", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", "Forest_cover", "Water_cover", "Cloud_cover")
  
  if (Year == 1990){
    ## writing metadata to matrix
    mat3 <- Write_metadata(mat = mat3 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Plot_ID = Plot_ID, Year = Year, BufferDistance = BufferDistance, Threshold = 30 )
    
    ## calculating forest cover raster
    K_1990 <- Kim_fun(Year = 19902000)
    
    ## applying SDM function to forest cover raster
    SDMK_1990 <- SDM_function(K_1990[[1]])
    
    ## reading number of columns from SDM function output
    SDMK_col <- ncol(SDMK_1990) + 11
    
    ## writing results to matrix
    mat3[i, 12:SDMK_col] <- SDMK_1990
    
    ## Forest cover calc
    FC_K_1990 <- Forest_cover(K_1990[[1]])
    mat3[i, 9] <- FC_K_1990
    ## Water
    mat3[i, 10] <- K_1990[[2]]
    ## Cloud
    mat3[i, 11] <- K_1990[[3]]
    
    
    
    ## assigning col names
    if (j < 1) {
      colnames(SDMK_1990) -> K_1990_colnames
      names(mat3) <- c(caption, K_1990_colnames)
    } else{
      
    } 
    
    ## create global variable of matrix
    mat3 <<- mat3
    
    matrix_list <- list(kim_1990 = mat3)

    # plotting the figures and writing to file
    Figure_output <- list(K_1990[[4]])
    plot_Figures(Figure_output,j)

    
  } else if (Year == 2000){
    ## writing metadata to matrix
    mat <- Write_metadata(mat = mat ,Countrycode = Countrycode, Chronosequence = Chronosequence, Plot_ID = Plot_ID, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold)
    
    ## calculating forest cover raster
    S <- Sexton(Year, Threshold)
    
    ## applying SDM function to forest cover raster
    SDMS_2000 <- SDM_function(S[[1]])
    
    ## reading number of columns from SDM function output
    SDMS_col <- ncol(SDMS_2000) + 11
    
    ## writing results to matrix
    mat[i, 12:SDMS_col] <- SDMS_2000
    
    ## Forest cover, water, cloud calc
    FC_S_2000 <- Forest_cover(S[[1]])
    mat[i, 9] <- FC_S_2000
    ## Water
    mat[i, 10] <- S[[2]]
    ## Cloud
    mat[i, 11] <- S[[3]]
    
    
    ## writing metadata to matrix
    mat1 <- Write_metadata(mat = mat1,Countrycode = Countrycode, Chronosequence = Chronosequence, Plot_ID = Plot_ID, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    
    ## calculating forest cover raster
    H <- Hansen(Threshold, Year)
    
    ## applying SDM function to forest cover raster
    SDMH <- SDM_function(H[[1]])
    
    ## reading number of columns from SDM function output
    SDMH_col <- ncol(SDMH) + 11
    
    ## writing results to matrix
    mat1[i, 12:SDMH_col] <- SDMH
    
    ## Forest cover calc
    FC_H_2000 <- Forest_cover(H[[1]])
    mat1[i, 9] <- FC_H_2000
    ## Water
    mat1[i, 10] <- H[[2]]
    ## Cloud
    mat1[i, 11] <- 0
    
    
    ## writing metadata to matrix
    mat2 <- Write_metadata(mat = mat2 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Plot_ID = Plot_ID, Year = Year, BufferDistance = BufferDistance, Threshold = 30 )
    
    ## calculating forest cover raster
    K_2000 <- Kim_fun(Year = 20002005, Data = 2000)
    
    ## applying SDM function to forest cover raster
    SDMK_2000 <- SDM_function(K_2000[[1]])
    
    ## reading number of columns from SDM function output
    SDMK_col <- ncol(SDMK_2000) + 11
    
    ## writing results to matrix
    mat2[i, 12:SDMK_col] <- SDMK_2000
    
    ## Forest cover calc
    FC_K_2000 <- Forest_cover(K_2000[[1]])
    mat2[i, 9] <- FC_K_2000
    ## Water
    mat2[i, 10] <- K_2000[[2]]
    ## Cloud
    mat2[i, 11] <- K_2000[[3]]
    
    
    
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
    
    matrix_list <- list(Sexton_2000 = mat, Hansen_2000 = mat1, Kim_2000 = mat2)

    
    ## listing raster files and creating forest cover figure
    #Plot_Raster <- list(S[[1]], H[[1]], K_2000[[1]])
    #Plot_function(Year = Year, BufferDistance = BufferDistance, Threshold = Threshold, Plot_Raster)
    
    # Resample Hansen dataset for similar dimensions
    H[[3]] <- resample(H[[3]], S[[4]], method = "ngb")
    
    # Create figure output
    Figure_output <- list(S[[4]], H[[3]], K_2000[[4]]) 
    plot_Figures(Figure_output,j)
    

    
    
    
  } else if (Year == 2005){
    ## writing metadata to matrix
    mat4 <- Write_metadata(mat = mat4 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Plot_ID = Plot_ID, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    
    ## calculating forest cover raster
    S <- Sexton(Year, Threshold)
    
    ## applying SDM function to forest cover raster
    SDMS_2005 <- SDM_function(S[[1]])
    
    ## reading number of columns from SDM function output
    SDMS_col <- ncol(SDMS_2005) + 11
    
    ## writing results to matrix
    mat4[i, 12:SDMS_col] <- SDMS_2005
    
    ## Forest cover calc
    FC_S_2005 <- Forest_cover(S[[1]])
    mat4[i, 9] <- FC_S_2005
    ## Water
    mat4[i, 10] <- S[[2]]
    ## Cloud
    mat4[i, 11] <- S[[3]]

    
    ## writing metadata to matrix
    mat5 <- Write_metadata(mat = mat5 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Plot_ID = Plot_ID, Year = Year, BufferDistance = BufferDistance, Threshold = 30 )
    
    ## calculating forest cover raster
    K_2005 <- Kim_fun(Year = 20002005, Data = 2005)
    
    ## applying SDM function to forest cover raster
    SDMK_2005 <- SDM_function(K_2005[[1]])
    
    ## reading number of columns from SDM function output
    SDMK_col <- ncol(SDMK_2005) + 11
    
    ## writing results to matrix
    mat5[i, 12:SDMK_col] <- SDMK_2005
    
    ## Forest cover calc
    FC_K_2005 <- Forest_cover(K_2005[[1]])
    mat5[i, 9] <- FC_K_2005
    ## Water
    mat5[i, 10] <- K_2005[[2]]
    ## Cloud
    mat5[i, 11] <- K_2005[[3]]
    
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
    
    matrix_list <- list(Sexton_2005 = mat4, Kim_2005 = mat5)
    
    # plotting the figures and writing to file
    Figure_output <- list(S[[4]], K_2005[[4]])
    plot_Figures(Figure_output,j)
    
  } else if (Year == 2012) {
    
    ## writing metadata to matrix
    mat6 <- Write_metadata(mat = mat6 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Plot_ID = Plot_ID, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    
    ## calculating forest cover raster
    H <- Hansen(Threshold, Year)
    
    ## applying SDM function to forest cover raster
    SDMH_2012 <- SDM_function(H[[1]])
    
    ## reading number of columns from SDM function output
    SDMH_col <- ncol(SDMH_2012) + 11
    
    ## writing results to matrix
    mat6[i, 12:SDMH_col] <- SDMH_2012
    
    ## Forest cover calc
    FC_H_2012 <- Forest_cover(H[[1]])
    mat6[i, 9] <- FC_H_2012
    ## Water
    mat6[i, 10] <- S[[2]]
    ## Cloud
    mat6[i, 11] <- NULL
    
    ## assigning col names
    if (j < 1) {
      colnames(SDMH_2012) -> SDMH_2012_colnames
      names(mat6) <- c(caption, SDMH_2012_colnames)
      
    } else {
      
    } 
    
    ## create global variable of matrix
    mat6 <<- mat6
    
    matrix_list <- list(Hansen_2012 = mat6)
    
    # plotting the figures and writing to file
    Figure_output <- list(H[[3]])
    plot_Figures(Figure_output,j)
    
  } else {
    warning("No valid year")
  }
  
  return(matrix_list)


}