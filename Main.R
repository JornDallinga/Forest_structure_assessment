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
if (!require(spatstat)) install.packages('car')
if (!require(spatstat)) install.packages('fsmb')
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
library (car)



###------------------------------------- Source Scripts ----------------------------------

source("R/Buffer_Coordinates.R")
source("R/Sexton.R")
source("R/Forest_Analysis.R")
source("R/Forest_cover.R")
source("R/Unpack_VCF.R")
source("R/Kim_fun.R")
source("R/Hansen.R")
source("R/SDMTool.R")
source("R/SDM_plot.R")
source("R/Write_data.R")
source("R/Write_Metadata.R")
source("R/Listing_files.R")
source("R/Mosaic_Raster.R")
source("R/Plotting.R")
source("R/calc_mean.R")
source("R/plot_Figures.R")
source("R/Write_chrono_excel.R")
source("R/vif_func.R")




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
Year <- 2000             # Only applies to Sexton script
BufferDistance <- 1000   # Distance in meters
Threshold <- 30           # Cells with values greater than threshold are classified as 'Forest'


setInternet2(use = TRUE) 

###------------------------------------- Create Matrix for results ----------------------

## reading excel file
mydata <- read.xlsx("Chrono_Coords_list_R_Ready.xlsx", 4)
countcoords <- nrow(mydata)


#creating empty matrix
mat <- matrix(, nrow = countcoords, ncol = 49)

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
  setWinProgressBar(pb, i, title=paste(round(i/countcoords*100, 0),
                                        "% done"))
  
  
  ## reading country code and chronosequence from mydata
  Countrycode <- levels(mydata$Country[1 + j])[mydata$Country[1 + j]]
  Chronosequence <- levels(mydata$Chronosequence[1 + j])[mydata$Chronosequence[1 + j]]
  Plot_ID <- as.character(mydata$Plot_ID[1 + j])

  ###------------------------------------ Run functions -----------------------------------

  ## Create buffer around point
  Buffer_Point(Countrycode, BufferDistance) #Places a .rds file in the data folder
  
  ## Analysis Forest data
  matrix_list <- Forest_Analysis(Year = Year, Countrycode = Countrycode, Chronosequence = Chronosequence, Plot_ID = Plot_ID, BufferDistance = BufferDistance, Threshold = Threshold)
  
  # Clear directory to prevent extraction errors
  unlink("data/BufferWGS.rds", recursive = FALSE)

  ## assigning looping variables
  j <- 1 + j
  
  ## write to excel and RDS and assign colunm names
  if (i == countcoords){
    Write_fun(matrix_list)
    mat_list <- lapply(matrix_list, calc_mean)
    lapply(1:length(mat_list), function(i) write.xlsx(mat_list[[i]], file = sprintf("output/Excel/mean_Buffer%s_Threshold%s_Year%s.xlsx", BufferDistance, Threshold, Year), sheetName = names(mat_list[i]), append = T))
    
    ## Writing individual chronosequences excel files to folder
    Write_chrono_excel(matrix_list)
    
    print("Done")
    close(pb)
  } else {
    print(sprintf("looping nr = %s", i))
  }
  
}
