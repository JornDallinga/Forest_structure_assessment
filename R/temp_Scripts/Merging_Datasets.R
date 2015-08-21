## reading data

Biomass_data = read.table("Dataset recovery biomass Jorn formatted.txt")
Species_data = read.table("Dataset recovery species richness Jorn formatted.txt")

head(Biomass_data)
Biomass_data[1]

write.xlsx(Biomass_data, file = "output/Excel/Biomass_data.xlsx", sheetName = "Biomass_data", append = T)
write.xlsx(Species_data, file = "output/Excel/Species_data.xlsx", sheetName = "Species_data", append = T)

Biomass_data <- read.xlsx("output/Excel/Biomass_data.xlsx", 1)
test <- read.xlsx("Mean_Year2000_buffer1000_threshold30.xlsx", 1)

## merging data frames
merge_test <- merge(test, Biomass_data, by = "Chronosequence", all.x = F, all.y = F, sort = T)
write.xlsx(merge_test, file = "output/Excel/merge.xlsx", sheetName = "merge", append = T)
head(merge_test)


## test linearity
### subset on biomass
subset_biomass <- subset(merge_test, variable == "biomass")
subset_biomass[,] <- sapply(subset_biomass[,], as.character, na.rm = T)
subset_biomass[,c(2,4,5,8:12,14:22)] <- sapply(subset_biomass[, c(2,4,5,8:12,14:22)], as.numeric, na.rm = TRUE)

## regression testing
fit <- lm(subset_biomass$pred20 ~ subset_biomass$precip + subset_biomass$T_CEC_SOIL5 + subset_biomass$Forest_cover, na.action = na.exclude)

plot(fit)
summary(fit)


