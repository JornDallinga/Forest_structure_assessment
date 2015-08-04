Write_fun <- function(Year){
  caption <- c(Countrycode, Chronosequence, BufferDistance, Year)
  dir.create(file.path('output/Excel'), showWarnings = FALSE)
  dir.create(file.path('output/RDS'), showWarnings = FALSE)
  if (Year == 1990){
    write.xlsx(Kim_1990, file = sprintf("output/Excel/%s_%s_Buffer%s_Year%s.xlsx", Countrycode, Chronosequence, BufferDistance, Year), sheetName = "Kim")
    saveRDS(Kim_1990,file = sprintf("output/RDS/%s_%s_Buffer%s_Year%s_Kim", Countrycode, Chronosequence, BufferDistance, Year))
    
  } else if (Year == 2000){ 
    write.xlsx(Sexton_2000, file = sprintf("output/Excel/%s_%s_Buffer%s_Year%s.xlsx", Countrycode, Chronosequence, BufferDistance, Year), sheetName = "Sexton")
    saveRDS(Sexton_2000,file = sprintf("output/RDS/%s_%s_Buffer%s_Year%s_Sexton", Countrycode, Chronosequence, BufferDistance, Year))
    write.xlsx(Hansen_2000, file = sprintf("output/Excel/%s_%s_Buffer%s_Year%s.xlsx", Countrycode, Chronosequence, BufferDistance, Year), sheetName = "Hansen", append = T)
    saveRDS(Hansen_2000,file = sprintf("output/RDS/%s_%s_Buffer%s_Year%s_Hansen", Countrycode, Chronosequence, BufferDistance, Year))
    write.xlsx(Kim_2000, file = sprintf("output/Excel/%s_%s_Buffer%s_Year%s.xlsx", Countrycode, Chronosequence, BufferDistance, Year), sheetName = "Kim", append = T)
    saveRDS(Kim_2000,file = sprintf("output/RDS/%s_%s_Buffer%s_Year%s_Kim", Countrycode, Chronosequence, BufferDistance, Year))
    
  } else if (Year == 2005){ 
    write.xlsx(Sexton_2005, file = sprintf("output/Excel/%s_%s_Buffer%s_Year%s.xlsx", Countrycode, Chronosequence, BufferDistance, Year), sheetName = "Sexton")
    saveRDS(Sexton_2005,file = sprintf("output/RDS/%s_%s_Buffer%s_Year%s_Sexton", Countrycode, Chronosequence, BufferDistance, Year))
    write.xlsx(Kim_2005, file = sprintf("output/Excel/%s_%s_Buffer%s_Year%s.xlsx", Countrycode, Chronosequence, BufferDistance, Year), sheetName = "Kim", append = T)
    saveRDS(Kim_2005,file = sprintf("output/RDS/%s_%s_Buffer%s_Year%s_Kim", Countrycode, Chronosequence, BufferDistance, Year))
    
  } else if (Year == 2012) {
    write.xlsx(Hansen_2012, file = sprintf("output/Excel/%s_%s_Buffer%s_Year%s.xlsx", Countrycode, Chronosequence, BufferDistance, Year), sheetName = "Hansen")
    saveRDS(Hansen_2012,file = sprintf("output/RDS/%s_%s_Buffer%s_Year%s_Hansen", Countrycode, Chronosequence, BufferDistance, Year))
  } else {
    print("cant write to excel or to RDS")
  }
}
