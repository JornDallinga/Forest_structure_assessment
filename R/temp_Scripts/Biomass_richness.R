## reading data

#Biomass_data = read.table("Dataset recovery biomass Jorn formatted.txt")

#head(Biomass_data)
#Biomass_data[1]

#write.xlsx(Biomass_data, file = "output/Excel/Biomass_data.xlsx", sheetName = "Biomass_data", append = T)
#write.xlsx(Species_data, file = "output/Excel/Species_data.xlsx", sheetName = "Species_data", append = T)

########################  Preparing data
Biomass_data <- read.xlsx("Biomass_data.xlsx", 1)
test <- read.xlsx("output/Excel/Mean_Buffer500_Threshold30_Year2000.xlsx", 1)

## merging data frames
merge_test <- merge(test, Biomass_data, by = "Chronosequence", all.x = F, all.y = F, sort = T)
head(merge_test)

## cleaning data fram
drops <- c("NA.", "Country.y", "X1", "X1.x", "X1.y", "Lat", "Long", "class", "Water_cover", "Cloud_cover")
merge_test <- merge_test[,!(names(merge_test) %in% drops)]

## writing to xl
#write.xlsx(merge_test, file = "output/Excel/merge.xlsx", sheetName = "merge_hansen", append = T)

## test linearity

## subset on biomass
subset_biomass <- subset(merge_test, variable == "biomass")
## converting to character
subset_biomass[,] <- sapply(subset_biomass[,], as.character, na.rm = T)
## Converting to numeric except for the new_drop variables
new_drop <- c("Chronosequence", "Country.x", "variable", "land.use")
subset_biomass[,!(names(subset_biomass) %in% new_drop)] <- sapply(subset_biomass[,!(names(subset_biomass) %in% new_drop)], as.numeric, na.rm = T)

# Selecting variables for regression
subset_biomass <- subset(subset_biomass, select = c("Forest_cover","patch.density", "edge.density", "landscape.shape.index", "aggregation.index", "effective.mesh.size", "patch.cohesion.index", "land.use", "precip", "T_CEC_SOIL5", "pred10", "pred20", "pred30", "pred40", "pred50"))

## Cleaning biomass data set for the value pred20. Removing NA's
subset_biomass <- subset_biomass[!is.na(subset_biomass$pred20),]
subset_biomass <- subset_biomass[!is.na(subset_biomass$Forest_cover),]

## Removing other pred values
subset_biomass <- subset_biomass[ , -which(names(subset_biomass) %in% c("pred10", "pred30", "pred40", "pred50"))]

# automate VIF selection
testing <- vif_func(in_frame= subset_biomass, thresh=5,trace=T)

## Linear Model
fit <- lm(subset_biomass$pred20 ~ subset_biomass$land.use + subset_biomass$precip + subset_biomass$T_CEC_SOIL5 + subset_biomass$patch.density + subset_biomass$edge.density + subset_biomass$landscape.shape.index + subset_biomass$patch.cohesion.index + subset_biomass$effective.mesh.size,data=mydata)
fit <- lm(subset_biomass$pred20 ~ rainfall + rainfall2)
rainfall2 <- rainfall ^2
plot(subset_biomass$pred20 ~ subset_biomass$effective.mesh.size)
summary(fit)

## ScatterplotMatrix
subset_nolanduse <- subset_biomass[-8]
scatterplotMatrix(subset_nolanduse)

## Stepwise Regression
fit <- lm(subset_biomass$pred20 ~ subset_biomass$land.use + subset_biomass$precip + subset_biomass$T_CEC_SOIL5 + subset_biomass$patch.density + subset_biomass$edge.density + subset_biomass$landscape.shape.index + subset_biomass$patch.cohesion.index + subset_biomass$effective.mesh.size,data=mydata)
fit <- lm(subset_biomass$pred20 ~ subset_biomass$precip + subset_biomass$T_CEC_SOIL5 + subset_biomass$edge.density + subset_biomass$patch.cohesion.index + subset_biomass$Forest_cover)

rainfall <- subset_biomass$precip ^ 2
step <- stepAIC(fit, direction="backward")
step$anova # display results

## check for collinearity
vif(fit)
vcov(fit)

  
###############################################PCA##############################################################
# PCA

## Standardise values
standardised_biomass <- as.data.frame(scale(subset_biomass[3:40]))
standardised_biomass <- as.data.frame(scale(subset_biomass[-which(names(subset_biomass) %in% c("land.use","pred20"))]))

# applying PCA analysis
Biomass.pca <- prcomp(na.omit(standardised_biomass))
summary(Biomass.pca)
Biomass.pca$x[,1]
  
plot(subset_biomass$pred20 ~ Biomass.pca$x[,1])

# How many PCA components to retain
screeplot(Biomass.pca, type="lines")  
  
  
biplot(Biomass.pca)







## regression testing
## predictor = Dependant variable
fit <- lm(subset_biomass$pred20 ~ subset_biomass$precip + subset_biomass$Forest_cover + subset_biomass$T_CEC_SOIL5 + subset_biomass$patch.cohesion.index, na.action = na.exclude)
fit <- lm(subset_biomass$pred20 ~ subset_biomass$land.use + subset_biomass$precip + subset_biomass$T_CEC_SOIL5 + subset_biomass$patch.cohesion.index, na.action = na.exclude)
fit <- lm(subset_biomass$pred20 ~ subset_biomass$Forest_cover, na.action = na.exclude)
fit <- lm(subset_biomass$pred20 ~ subset_biomass$Patch_cohesion, na.action = na.exclude)

plot(subset_biomass$pred20 ~ subset_biomass$)
plot(fit)
summary(fit)

## ScatterplotMatrix

scatterplotMatrix(subset_biomass[6:8])

## Standardise values
standardised_biomass <- as.data.frame(scale(subset_biomass[3:40]))
standardised_biomass[7:8] <- as.data.frame(scale(subset_biomass[16:17]))


## linear plotting of standardised biomass values
fit <- lm(subset_biomass_pred20$pred20 ~ subset_biomass_pred20$land.use + standardised_biomass$precip + standardised_biomass$T_CEC_SOIL5 + standardised_biomass$Patch_cohesion, na.action = na.exclude)
plot(fit)
summary(fit)

# prepare for PCA analysis
Biomass.Cleaned <- standardised_biomass[complete.cases(standardised_biomass),]
Biomass.Cleaned <- Biomass.Cleaned[ , -which(names(Biomass.Cleaned) %in% c("Cloud_cover","Water_cover"))]

# applying PCA analysis
Biomass.pca <- prcomp(na.omit(Biomass.Cleaned))
summary(Biomass.pca)
Biomass.pca$x[,1]

## check PCA variances == above 1 should be retained
(Biomass.pca$sdev)^2

### Values of first PCA component
PCA_fit <- lm(subset_biomass$pred20 ~ Biomass.pca$x[,1] + Biomass.pca$x[,2])


subset_biomass_pred20)
Biomass.pca$x[,1]

nrow(subset_biomass_pred20)
length(Biomass.pca$x[,1])

# VIF testing
VIF_frame <- subset.biomass[ , -which(names(subset_biomass) %in% c("Cloud_cover","Water_cover", "Chronosequence", "Country.x", "Class", ""))]
VIF_frame <- subset(subset_biomass, select = c("Forest_cover","patch.density", "edge.density", "landscape.shape.index", "aggregation.index", "effective.mesh.size", "patch.cohesion.index"))
testing <- vif_func(in_frame= VIF_frame,thresh=5,trace=T)

# How many PCA components to retain
screeplot(Biomass.pca, type="lines")

# 

# non-linearity
crPlots(fit)
ceresPlots(fit)

## save sexton data
Sexton_fit <- fit
Hansen_fit <- fit
Kim_fit <- fit

summary(Sexton_fit)
summary(Hansen_fit)
summary(Kim_fit)

## test for significant different between models
Anova(Sexton_fit, Hansen_fit)


