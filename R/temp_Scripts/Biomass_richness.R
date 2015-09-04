## reading data

Biomass_data = read.table("Dataset recovery biomass Jorn formatted.txt")
Species_data = read.table("Dataset recovery species richness Jorn formatted.txt")

head(Biomass_data)
Biomass_data[1]

write.xlsx(Biomass_data, file = "output/Excel/Biomass_data.xlsx", sheetName = "Biomass_data", append = T)
write.xlsx(Species_data, file = "output/Excel/Species_data.xlsx", sheetName = "Species_data", append = T)

Biomass_data <- read.xlsx("Biomass_data.xlsx", 1)
test <- read.xlsx("output/Excel/Mean_Buffer1000_Threshold30_Year2000.xlsx", 1)

## merging data frames
merge_test <- merge(test, Biomass_data, by = "Chronosequence", all.x = F, all.y = F, sort = T)
head(merge_test)

## cleaning data fram
drops <- c("NA.", "Country.y", "X1")
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


## regression testing
## predictor = Dependant variable
fit <- lm(subset_biomass$pred20 ~ subset_biomass$precip + subset_biomass$Forest_cover + subset_biomass$T_CEC_SOIL5 + subset_biomass$Patch_cohesion, na.action = na.exclude)
fit <- lm(subset_biomass$pred20 ~ subset_biomass$land.use + subset_biomass$precip + subset_biomass$T_CEC_SOIL5 + subset_biomass$Patch_cohesion, na.action = na.exclude)
fit <- lm(subset_biomass$pred20 ~ subset_biomass$Forest_cover, na.action = na.exclude)
fit <- lm(subset_biomass$pred20 ~ subset_biomass$Patch_cohesion, na.action = na.exclude)

plot(subset_biomass$pred30 ~ subset_biomass$precip)
plot(fit)
summary(fit)

## check for collinearity
vif(fit)

# automate VIF selection

VIF_frame <- subset(subset_biomass, select = c("precip","Forest_cover", "T_CEC_SOIL5", "Patch_cohesion"))

testing <- vif_func(in_frame= VIF_frame,thresh=8,trace=T)

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


