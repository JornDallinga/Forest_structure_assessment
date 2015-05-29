####################################### TEAM DDJ #######################################
### Developed by: Jeroen Roelofs, Daniel Scheerooren & Jorn Dallinga
### GeoScripting Course, Wageningen University
### Modification date: 29-01-2015

###################################### Main script ######################################

### --------------------------------- Aquire and load libraries ------------------------

### Install packages if required.
if (!require(SDMTools)) install.packages('SDMTools')
if (!require(gfcanalysis)) install.packages('gfcanalysis')
if (!require(rgdal)) install.packages('xlsx')
if (!require(rgdal)) install.packages('rgdal')
if (!require(rgeos)) install.packages('rgeos')
if (!require(sp)) install.packages('sp')
if (!require(raster)) install.packages('raster')
if (!require(spatstat)) install.packages('spatstat')
if (!require(devtools)) install.packages("devtools")
if (!require(MODIS)) install.packages("MODIS", repos="http://R-Forge.R-project.org")
devtools::install_github('dutri001/VCF')

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
source("R/BufferRandomPointsInCountry.R")
source("R/Buffer_Coordinates.R")
source("R/Sexton.R")
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
if (file.exists('extract_sexton')){
} else {
  dir.create(file.path('data/extract_sexton'), showWarnings = FALSE)
}

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
Threshold <- 70           # Cells with values greater than threshold are classified as 'Forest'



###------------------------------------- Create Matrix for results ----------------------

## reading excel file
mydata <- read.xlsx("Correct_excel.xlsx", 1)
countcoords <- nrow(mydata)

#creating empty matrix
mat <- matrix(, nrow = countcoords, ncol = 5)
colnames(mat) <- c("Buffer", "x_coordinates", "y_coordinates", "Sexton", "Hansen")

#adding names of the buffers to the matrix
for(i in 1:countcoords) {
  nam <- paste("Buffer size: ", BufferDistance, ", year: ", Year , ", Thres: ", Threshold, ", nr: ", i,  sep = "")
  mat[i] <- nam
}


###------------------------------------- Create loops -----------------------------------
count <- 2
for(i in 1:countcoords) {
  

  ###------------------------------------ Run functions -----------------------------------
  
  Buffer_Point(Countrycode, BufferDistance) #Places a .rds file in the output folder
  
  ## adding Coordinates to the matrix
  mat[i, count] <- mydata$x[3 - i]
  mat[i, count + 1] <- mydata$y[1]
  
  ## Analysis with sexton data
  S <- Sexton(Year, Threshold)
  
  ## Species Distribution Modelling for both Sexton and Hansen data
  SDMS <- SDM_function(S)
  
  #Adding Sexton results to the matrix
  mat[i, count] <- SDMS
  count <- count + 1
  
  ## Global Forest Cover Analysis with Hansen data
  H <- Hansen(Threshold)
  
  ## Species Distribution Modelling for both Sexton and Hansen data
  SDMH <- SDM_function(H)
  
  #Adding Hansen results to the matrix
  mat[i, count] <- SDMH
  count <- count - 2
  
  New_S <-projectRaster(S, H, res, crs, method="ngb", 
                        alignOnly=FALSE, over=FALSE, filename="")
  
}


# Assign plot fragmentation function to variables
plot_Sexton <- SDM_plot(S)
plot_Hansen <- SDM_plot(H)

New_proj_Sexton <-projectRaster(plot_Sexton, plot_Hansen, res, crs, method="ngb", 
                      alignOnly=FALSE, over=FALSE, filename="")

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
