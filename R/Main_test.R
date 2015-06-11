####################################### TEAM DDJ #######################################
### Developed by: Jeroen Roelofs, Daniel Scheerooren & Jorn Dallinga
### GeoScripting Course, Wageningen University
### Modification date: 29-01-2015

###################################### Main script ######################################

### --------------------------------- Aquire and load libraries ------------------------

### Install packages if required.
if (!require(SDMTools)) install.packages('SDMTools')
if (!require(xlsx)) install.packages('xlsx')
if (!require(gfcanalysis)) install.packages('gfcanalysis')
if (!require(rgdal)) install.packages('rgdal')
if (!require(rgeos)) install.packages('rgeos')
if (!require(sp)) install.packages('sp')
if (!require(spatstat)) install.packages('spatstat')
if (!require(devtools)) install.packages("devtools")
if (!require(MODIS)) install.packages("MODIS", repos="http://R-Forge.R-project.org")
devtools::install_github('JornDallinga/VCF')


### Access package libraries
library (SDMTools)
library (gfcanalysis)
library (rgdal)
library (rgeos)
library (sp) 
library (spatstat)
library (devtools)
library (VCF)
library (plyr)
library (xlsx)


###------------------------------------- Source Scripts ----------------------------------

source("R/Buffer_Coordinates.R")
source("R/Sexton.R")
source("R/colnames.R")
source("R/Unpack_VCF.R")
source("R/Kim_1990.R")
source("R/Kim_2000.R")
source("R/Kim_2005.R")
source("R/Hansen.R")
source("R/SDMTool.R")
source("R/SDM_plot.R")


###------------------------------------- Create folders ----------------------------------

### Create data and output folders if necessary 
## Data folder
if (file.exists('data')){
} else {
  dir.create(file.path('data'), showWarnings = FALSE)  
}

## Output folder
if (file.exists('output')){
} else {
  dir.create(file.path('output'), showWarnings = FALSE)
}

## Output folder
#if (file.exists('extract_sexton')){
#} else {
#  dir.create(file.path('data/extract_sexton'), showWarnings = FALSE)
#}

## Output folder
if (file.exists('extract_hansen')){
} else {
  dir.create(file.path('data/extract_hansen'), showWarnings = FALSE)
}

###------------------------------------- Set variables -----------------------------------
### Set variables by user
Countrycode <- "CRI"      # See: http://en.wikipedia.org/wiki/ISO_3166-1
Year <- 2000              # Only applies to Sexton script
BufferDistance <- 1000    # Distance in meters
Threshold <- 30           # Cells with values greater than threshold are classified as 'Forest'

setInternet2(use = TRUE) 

###------------------------------------- Create Matrix for results ----------------------

## reading excel file
mydata <- read.xlsx("Correct_excel.xlsx", 1)
countcoords <- nrow(mydata)


#creating empty matrix
mat <- matrix(, nrow = countcoords, ncol = 44)

#creating empty dataframe
mat <- data.frame(mat)


#adding names of the buffers to the matrix

for(i in 1:countcoords) {
  nam <- paste(Countrycode)
  nam1 <- paste(Year)
  nam2 <- paste(BufferDistance)
  nam3 <- paste(Threshold)
  
  mat[1] <- nam
  mat[2] <- nam1
  mat[3] <- nam2
  mat[4] <- nam3
}

mat -> mat1 -> mat2 -> mat3 -> mat4 -> mat5

###------------------------------------- Create loops -----------------------------------
count <- 5
j <- 0

for(i in 1:countcoords) {
  
  
  ###------------------------------------ Run functions -----------------------------------
  
  Buffer_Point(Countrycode, BufferDistance) #Places a .rds file in the data folder
  
  ## Analysis with sexton data
  if (Year == 1990){
    K_1990 <- Kim_1990(Year = 19902000)
    SDMK_1990 <- SDM_function(K_1990)
    SDMK_col <- ncol(SDMK_1990) + 6
    mat3[i, 7:SDMK_col] <- SDMK_col
    
  } else if (Year == 2000){
    S <- Sexton(Year, Threshold)
    SDMS_2000 <- SDM_function(S)
    SDMS_col <- ncol(SDMS_2000) + 6
    mat[i, 7:SDMS_col] <- SDMS_2000
    
    H <- Hansen(Threshold)
    SDMH <- SDM_function(H)
    SDMH_col <- ncol(SDMH) + 6
    mat1[i, 7:SDMH_col] <- SDMH
    
    K_2000 <- Kim_2000(Year = 20002005)
    SDMK_2000 <- SDM_function(K_2000)
    SDMK_col <- ncol(SDMK_2000) + 6
    mat2[i, 7:SDMK_col] <- SDMK_2000
    
    
  } else if (Year == 2005){
    S <- Sexton(Year, Threshold)
    SDMS_2005 <- SDM_function(S)
    SDMS_col <- ncol(SDMS_2005) + 6
    mat4[i, 7:SDMS_col] <- SDMS_2005
    
    K_2005 <- Kim_2005(Year = 20002005)
    SDMK_2005 <- SDM_function(K_2005)
    SDMK_col <- ncol(SDMK_2005) + 6
    mat5[i, 7:SDMK_col] <- SDMK_2005
  } else {
    print("No valid year")
  }

  # Clear directory to prevent extraction errors
  unlink("data/BufferWGS.rds", recursive = FALSE)
  
  ## adding column names based on output SDMTools function
  if (j < 1 & Year == 1990) {
    colnames(SDMK_1990) -> K_1990_colnames
    names(mat3) <- c("Country", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", K_1990_colnames)
  } else if (j < 1 & Year == 2000) {
    colnames(SDMS_2000) -> SDMS_2000_colnames
    names(mat) <- c("Country", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", SDMS_2000_colnames)
    colnames(SDMK_2000) -> K_2000_colnames
    names(mat2) <- c("Country", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", K_2000_colnames)
    colnames(SDMH) -> SDMH_colnames
    names(mat1) <- c("Country", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", SDMH_colnames)
  } else if (j < 1 & Year == 2005) {
    colnames(SDMS_2005) -> SDMS_2005_colnames
    names(mat4) <- c("Country", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", SDMS_2005_colnames)
    colnames(SDMK_2005) -> K_2005_colnames
    names(mat5) <- c("Country", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", K_2005_colnames)
  } else {
    
  }
  

  ## adding Coordinates to the matrix
  mat[i, count] <- mydata$x[1 + j]
  mat1[i, count] <- mydata$x[1 + j]
  mat2[i, count] <- mydata$x[1 + j]
  mat3[i, count] <- mydata$x[1 + j]
  mat4[i, count] <- mydata$x[1 + j]
  mat5[i, count] <- mydata$x[1 + j]
  count <- count + 1
  mat[i, count] <- mydata$y[1 + j]
  mat1[i, count] <- mydata$y[1 + j]
  mat2[i, count] <- mydata$y[1 + j]
  mat3[i, count] <- mydata$y[1 + j]
  mat4[i, count] <- mydata$y[1 + j]
  mat5[i, count] <- mydata$y[1 + j]
  
  ## assigning looping variables
  j <- 1 + j
  count <- count - 1
  
}

# write data to excel
if (Year == 1990){
  write.xlsx(mat3, file = sprintf("output/%s_buffer%s_year%s.xlsx", Countrycode, BufferDistance, Year), sheetName = "Kim")
  
} else if (Year == 2000){ 
  write.xlsx(mat, file = sprintf("output/%s_buffer%s_year%s.xlsx", Countrycode, BufferDistance, Year), sheetName = "Sexton")
  write.xlsx(mat1, file = sprintf("output/%s_buffer%s_year%s.xlsx", Countrycode, BufferDistance, Year), sheetName = "Hansen", append = T)
  write.xlsx(mat2, file = sprintf("output/%s_buffer%s_year%s.xlsx", Countrycode, BufferDistance, Year), sheetName = "Kim", append = T)
  
} else if (Year == 2005){ 
  write.xlsx(mat4, file = sprintf("output/%s_buffer%s_year%s.xlsx", Countrycode, BufferDistance, Year), sheetName = "Sexton")
  write.xlsx(mat5, file = sprintf("output/%s_buffer%s_year%s.xlsx", Countrycode, BufferDistance, Year), sheetName = "Kim", append = T)
  
} else {
  print("cant write to excel")
}


# Assign plot fragmentation function to variables
plot_Sexton <- SDM_plot(S)
plot_Hansen <- SDM_plot(H)

#New_proj_Sexton <-projectRaster(plot_Sexton, plot_Hansen, res, crs, method="ngb", 
#                                alignOnly=FALSE, over=FALSE, filename="")

png(filename="output/Fragmentation.png")
par(mfrow=c(2,2))

# Plot forest/non-forest
plot(New_S, main = "Sexton")
plot(H, main = "Hansen")

# Plot the fragmentation of Sexton and Hansen
plot(New_proj_Sexton, main = "Sexton")
plot(plot_Hansen, main = "Hansen")
dev.off()

###------------------------------------ Summerize/Visualize output -----------------------

## Compare output Sexton and Hansen data
## Visualize in scatterplot
png(filename="output/Scatterplot.png")
plot(mat[,2], mat[,3], main = "Difference in nr. of patches between Sexton and Hansen data", xlab="Sexton", ylab="Hansen", col = "red" )
dev.off()
