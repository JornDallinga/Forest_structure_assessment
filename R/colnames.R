columnnames <- function(Year) {
  if (Year == 1990) {
    colnames(SDMK_1990) -> K_1990_colnames
    names(mat3) <- c("Country", "Chronosequence", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", K_1990_colnames)
  } else if (Year == 2000) {
    colnames(SDMS_2000) -> SDMS_2000_colnames
    names(mat) <- c("Country","Chronosequence", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", SDMS_2000_colnames)
    colnames(SDMK_2000) -> K_2000_colnames
    names(mat2) <- c("Country","Chronosequence", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", K_2000_colnames)
    colnames(SDMH) -> SDMH_colnames
    names(mat1) <- c("Country","Chronosequence", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", SDMH_colnames)
  } else if (Year == 2005) {
    colnames(SDMS_2005) -> SDMS_2005_colnames
    names(mat4) <- c("Country","Chronosequence", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", SDMS_2005_colnames)
    colnames(SDMK_2005) -> K_2005_colnames
    names(mat5) <- c("Country","Chronosequence", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", K_2005_colnames)
  } else if (Year == 2012) {
    colnames(SDMH_2012) -> SDMH_2012_colnames
    names(mat6) <- c("Country","Chronosequence", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", SDMH_2012_colnames)
  } else {
    
  }
  
}
