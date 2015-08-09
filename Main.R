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
source("R/Forest_cover.R")
source("R/Unpack_VCF.R")
source("R/Kim_fun_1990.R")
source("R/Kim_fun_2000.R")
source("R/Kim_fun_2005.R")
source("R/Hansen.R")
source("R/SDMTool.R")
source("R/SDM_plot.R")
source("R/Write_data.R")
source("R/Write_Metadata.R")
source("R/Listing_files.R")
source("R/Mosaic_Raster.R")
source("R/Plotting.R")
source("R/calc_mean.R")




###------------------------------------- Create folders ----------------------------------

### Create data and output folders if necessary 
## Data folder

dir.create(file.path('data'), showWarnings = FALSE)
dir.create(file.path('output'), showWarnings = FALSE)
dir.create(file.path('data/extract_hansen'), showWarnings = FALSE)


###------------------------------------- Set variables -----------------------------------
### Set variables by user
#Countrycode <- "CRI"      # See: http://en.wikipedia.org/wiki/ISO_3166-1
#Chronosequence <- NULL    # Chronosequence within the country
Year <- 2000            # Only applies to Sexton script
BufferDistance <- 1000    # Distance in meters
Threshold <- 30           # Cells with values greater than threshold are classified as 'Forest'


setInternet2(use = TRUE) 

###------------------------------------- Create Matrix for results ----------------------

## reading excel file
mydata <- read.xlsx("Chrono_Coords_list_R_Ready.xlsx", 3)
countcoords <- nrow(mydata)


#creating empty matrix
mat <- matrix(, nrow = countcoords, ncol = 46)

#creating empty dataframe
mat <- data.frame(mat)

# Copying data frames to equal the amount of databases
mat -> mat1 -> mat2 -> mat3 -> mat4 -> mat5 -> mat6


###------------------------------------- Create loops -----------------------------------
count <- 1
j <- 0


###------------------------------------- Run Script -----------------------------------

for(i in 1:countcoords) {
  
  # create progress bar
  pb <- winProgressBar(title = "progress bar", min = 0,
                       max = countcoords, width = 300)
  Sys.sleep(0.1)
  setWinProgressBar(pb, i, title=paste( round(i/countcoords*100, 0),
                                        "% done"))
  
  
  ## reading country code and chronosequence from mydata
  Countrycode <- levels(mydata$Country[1 + j])[mydata$Country[1 + j]]
  Chronosequence <- levels(mydata$Chronosequence[1 + j])[mydata$Chronosequence[1 + j]]

  ###------------------------------------ Run functions -----------------------------------

  ## Create buffer around point
  Buffer_Point(Countrycode, BufferDistance) #Places a .rds file in the data folder
  
  ## Analysis Forest data
  matrix_list <- Forest_Analysis(Year = Year, Countrycode = Countrycode, Chronosequence = Chronosequence, BufferDistance = BufferDistance, Threshold = Threshold)
  
  # Clear directory to prevent extraction errors
  unlink("data/BufferWGS.rds", recursive = FALSE)

  ## assigning looping variables
  j <- 1 + j
  

  ## write to excel and RDS and assign colunm names
  if (i == countcoords){
    Write_fun(matrix_list)
    mat_list <- lapply(matrix_list, calc_mean)
    lapply(1:length(mat_list), function(i) write.xlsx(mat_list[[i]], file = sprintf("output/Excel/mean_Buffer%s_Threshold%s_%s.xlsx", BufferDistance, Threshold, names(mat_list[i]))))
    print("Done")
    close(pb)
  } else {
    print("looping again")
  }
  
}




###------------------------------------- Testing area -----------------------------------

# Assign plot fragmentation function to variables
plot_Sexton <- SDM_plot(S)
plot_Hansen <- SDM_plot(H)

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
