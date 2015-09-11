## reading data

#Species_data = read.table("Dataset recovery species richness Jorn formatted.txt")

head(Species_data)

#write.xlsx(Biomass_data, file = "output/Excel/Biomass_data.xlsx", sheetName = "Biomass_data", append = T)
#write.xlsx(Species_data, file = "output/Excel/Species_data.xlsx", sheetName = "Species_data", append = T)

Species_data <- read.xlsx("Species_data.xlsx", 1)
test <- read.xlsx("output/Excel/Mean_Buffer500_Threshold30_Year2000.xlsx", 1)

## merging data frames
merge_Species <- merge(test, Species_data, by = "Chronosequence", all.x = F, all.y = F, sort = T)
head(merge_Species)

## cleaning data fram
drops <- c("NA.", "Country.y", "X1", "X1.x", "X1.y", "Lat", "Long", "class", "Water_cover", "Cloud_cover")
merge_Species <- merge_Species[,!(names(merge_Species) %in% drops)]

## writing to xl
#write.xlsx(merge_test, file = "output/Excel/merge.xlsx", sheetName = "merge_hansen", append = T)

## test linearity

## subset on biomass
subset_species<- subset(merge_Species, variable == "Srar10.perc")
## converting to character
subset_species[,] <- sapply(subset_species[,], as.character, na.rm = T)
## Converting to numeric except for the new_drop variables
new_drop <- c("Chronosequence", "Country.x", "variable", "land.use")
subset_species[,!(names(subset_species) %in% new_drop)] <- sapply(subset_species[,!(names(subset_species) %in% new_drop)], as.numeric, na.rm = T)


## regression testing
## predictor = Dependant variable
fit <- lm(subset_species$pred20 ~ subset_species$precip + subset_species$Forest_cover + subset_species$T_CEC_SOIL5 + subset_species$Patch_cohesion, na.action = na.exclude)
fit <- lm(subset_species$pred20 ~ subset_species$land.use + subset_species$precip + subset_species$T_CEC_SOIL5 + subset_species$Forest_cover,  na.action = na.exclude)
fit <- lm(subset_species$pred20 ~ subset_species$Forest_cover, na.action = na.exclude)
fit <- lm(subset_species$pred20 ~ subset_species$edge.density + subset_species$Forest_cover, na.action = na.exclude)

plot(subset_species$pred20 ~ subset_species$edge.density + subset_species$Forest_cover)
plot(fit)
summary(fit)

## Scatterplot prep
subset_species_matrix <- subset_species[ , -which(names(subset_species) %in% c("land.use", "Country.x", "variable", "Chronosequence"))]
subset_species_matrix <- subset(subset_species_matrix, select = c("Forest_cover","n.patches","patch.density", "edge.density", "landscape.shape.index", "aggregation.index", "effective.mesh.size", "patch.cohesion.index", "precip", "T_CEC_SOIL5", "pred40"))

scatterplotMatrix(subset_species_matrix)

## check for collinearity
vif(fit)

# automate VIF selection

VIF_frame <- subset(subset_species, select = c("precip","Forest_cover", "T_CEC_SOIL5", "Patch_cohesion"))

testing <- vif_func(in_frame= VIF_frame,thresh=3,trace=T)

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


## standardize data?

scaled.dat <- scale(subset_species[,c("precip","Forest_cover", "T_CEC_SOIL5", "Patch_cohesion")])

# check that we get mean of 0 and sd of 1
colMeans(scaled.dat)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.dat, 2, sd)

fit <- lm(subset_species$pred20 ~ scaled.dat, na.action = na.exclude)
summary(fit)
