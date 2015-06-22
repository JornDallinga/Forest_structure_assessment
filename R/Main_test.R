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
source("R/Forest_Analysis.R")
source("R/Unpack_VCF.R")
source("R/Kim_1990.R")
source("R/Kim_2000.R")
source("R/Kim_2005.R")
source("R/Hansen.R")
source("R/SDMTool.R")
source("R/SDM_plot.R")
source("R/Write_Excel_RDS.R")


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
Year <- 2012              # Only applies to Sexton script
BufferDistance <- 1000    # Distance in meters
Threshold <- 70           # Cells with values greater than threshold are classified as 'Forest'

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

mat -> mat1 -> mat2 -> mat3 -> mat4 -> mat5 -> mat6

###------------------------------------- Create loops -----------------------------------
count <- 5
j <- 0


for(i in 1:countcoords) {
  
  
  ###------------------------------------ Run functions -----------------------------------
  
  Buffer_Point(Countrycode, BufferDistance) #Places a .rds file in the data folder
  
  ## Analysis Forest data
  Forest_Analysis(Year)
  
  # Clear directory to prevent extraction errors
  unlink("data/BufferWGS.rds", recursive = FALSE)

  ## adding Coordinates to the matrix
  mat[i, count] <- mydata$x[1 + j]
  mat1[i, count] <- mydata$x[1 + j]
  mat2[i, count] <- mydata$x[1 + j]
  mat3[i, count] <- mydata$x[1 + j]
  mat4[i, count] <- mydata$x[1 + j]
  mat5[i, count] <- mydata$x[1 + j]
  mat6[i, count] <- mydata$x[1 + j]
  count <- count + 1
  mat[i, count] <- mydata$y[1 + j]
  mat1[i, count] <- mydata$y[1 + j]
  mat2[i, count] <- mydata$y[1 + j]
  mat3[i, count] <- mydata$y[1 + j]
  mat4[i, count] <- mydata$y[1 + j]
  mat5[i, count] <- mydata$y[1 + j]
  mat6[i, count] <- mydata$x[1 + j]


  ## assigning looping variables
  j <- 1 + j
  count <- count - 1

  
  ## write to excel and RDS and assign colunm names
  if (i == countcoords){
    Write_fun(Year)
    print("Done")
  } else {
    print("looping again")
  }
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
