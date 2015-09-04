## reading data

Biomass_data = read.table("Dataset recovery biomass Jorn formatted.txt")
Species_data = read.table("Dataset recovery species richness Jorn formatted.txt")

head(Biomass_data)
Biomass_data[1]

write.xlsx(Biomass_data, file = "output/Excel/Biomass_data.xlsx", sheetName = "Biomass_data", append = T)
write.xlsx(Species_data, file = "output/Excel/Species_data.xlsx", sheetName = "Species_data", append = T)

Species_data <- read.xlsx("Species_data.xlsx", 1)
test <- read.xlsx("output/Excel/Mean_Buffer1000_Threshold30_Year2000.xlsx", 1)

## merging data frames
merge_Species <- merge(test, Species_data, by = "Chronosequence", all.x = F, all.y = F, sort = T)
head(merge_Species)

## cleaning data fram
drops <- c("NA.", "Country.y", "X1", "X1.x", "X1.y")
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
fit <- lm(subset_species$pred20 ~ subset_species$land.use + subset_species$precip + subset_species$T_CEC_SOIL5 + subset_species$n_patches,  na.action = na.exclude)
fit <- lm(subset_species$pred20 ~ subset_species$Forest_cover, na.action = na.exclude)
fit <- lm(subset_species$pred20 ~ (log10(subset_species$Patch_cohesion)), na.action = na.exclude)

plot(subset_species$pred20 ~ subset_species$Patch_cohesion)
plot(fit)
summary(fit)

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
