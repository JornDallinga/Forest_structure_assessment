Forest_Analysis <- function(Year = Year, Countrycode = Countrycode, Chronosequence = Chronosequence, BufferDistance = BufferDistance, Threshold = Threshold){
  
  caption <- c("Country","Chronosequence", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates")
  
  if (Year == 1990){
    mat3 <- Write_metadata(mat = mat3 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    K_1990 <- Kim_1990(Year = 19902000)
    SDMK_1990 <- SDM_function(K_1990)
    SDMK_col <- ncol(SDMK_1990) + 7
    mat3[i, 8:SDMK_col] <- SDMK_col
    
    if (j < 1) {
      colnames(SDMK_1990) -> K_1990_colnames
      names(mat3) <- c(caption, K_1990_colnames)
    } else{
      
    } 
    mat3 <<- mat3
    
  } else if (Year == 2000){
    mat <- Write_metadata(mat = mat ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    S <- Sexton(Year, Threshold)
    SDMS_2000 <- SDM_function(S)
    SDMS_col <- ncol(SDMS_2000) + 7
    mat[i, 8:SDMS_col] <- SDMS_2000
    
    mat1 <- Write_metadata(mat = mat1 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    H <- Hansen(Threshold, Year)
    SDMH <- SDM_function(H)
    SDMH_col <- ncol(SDMH) + 7
    mat1[i, 8:SDMH_col] <- SDMH
    
    mat2 <- Write_metadata(mat = mat2 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    K_2000 <- Kim_2000(Year = 20002005)
    SDMK_2000 <- SDM_function(K_2000)
    SDMK_col <- ncol(SDMK_2000) + 7
    mat2[i, 8:SDMK_col] <- SDMK_2000
    
    if (j < 1) {
      colnames(SDMS_2000) -> SDMS_2000_colnames
      names(mat) <- c(caption, SDMS_2000_colnames)
      colnames(SDMK_2000) -> K_2000_colnames
      names(mat2) <- c(caption, K_2000_colnames)
      colnames(SDMH) -> SDMH_colnames
      names(mat1) <- c(caption, SDMH_colnames)
    } else{
      
    } 
    mat <<- mat
    mat1 <<- mat1
    mat2 <<- mat2
    
  } else if (Year == 2005){
    mat4 <- Write_metadata(mat = mat4 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    S <- Sexton(Year, Threshold)
    SDMS_2005 <- SDM_function(S)
    SDMS_col <- ncol(SDMS_2005) + 7
    mat4[i, 8:SDMS_col] <- SDMS_2005
    
    mat5 <- Write_metadata(mat = mat5 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    K_2005 <- Kim_2005(Year = 20002005)
    SDMK_2005 <- SDM_function(K_2005)
    SDMK_col <- ncol(SDMK_2005) + 7
    mat5[i, 8:SDMK_col] <- SDMK_2005
    
    if (j < 1) {
      colnames(SDMS_2005) -> SDMS_2005_colnames
      names(mat4) <- c(caption, SDMS_2005_colnames)
      colnames(SDMK_2005) -> K_2005_colnames
      names(mat5) <- c(caption, K_2005_colnames)
    } else {
      
    } 
    mat4 <<- mat4
    mat5 <<- mat5
    
  } else if (Year == 2012) {
    mat6 <- Write_metadata(mat = mat6 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    H <- Hansen(Threshold, Year)
    SDMH_2012 <- SDM_function(H)
    SDMH_col <- ncol(SDMH_2012) + 7
    mat6[i, 8:SDMH_col] <- SDMH_2012
    if (j < 1) {
      colnames(SDMH_2012) -> SDMH_2012_colnames
      names(mat6) <- c(caption, SDMH_2012_colnames)
      
    } else{
      
    } 
    mat6 <<- mat6
    
  } else {
    warning("No valid year")
  }

  
}