my_sheet <- read.xlsx("output/Excel/Buffer1000_Threshold30_Year2000.xlsx", "Metadata")
my_sheet[3]
test <- list.files(path = "output/Excel/Chronosequences",
           recursive=T,
           pattern= ".xlsx"
           ,full.names=T)



test[1]

lapply(1:length(test), function(i) write.xlsx(my_sheet, file = test[i], sheetName = "Metadata", append = T))