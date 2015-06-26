Write_fun <- function(Year){
  caption <- c(Countrycode, Chronosequence, BufferDistance, Year)
  dir.create(file.path('output/Excel'), showWarnings = FALSE)
  dir.create(file.path('output/RDS'), showWarnings = FALSE)
  if (Year == 1990){
    write.xlsx(mat3, file = sprintf("output/Excel/%s_%s_Buffer%s_Year%s.xlsx", Countrycode, Chronosequence, BufferDistance, Year), sheetName = "Kim")
    saveRDS(mat3,file = sprintf("output/RDS/%s_%s_Buffer%s_Year%s_Kim", Countrycode, Chronosequence, BufferDistance, Year))
    
  } else if (Year == 2000){ 
    write.xlsx(mat, file = sprintf("output/Excel/%s_%s_Buffer%s_Year%s.xlsx", Countrycode, Chronosequence, BufferDistance, Year), sheetName = "Sexton")
    saveRDS(mat,file = sprintf("output/RDS/%s_%s_Buffer%s_Year%s_Sexton", Countrycode, Chronosequence, BufferDistance, Year))
    write.xlsx(mat1, file = sprintf("output/Excel/%s_%s_Buffer%s_Year%s.xlsx", Countrycode, Chronosequence, BufferDistance, Year), sheetName = "Hansen", append = T)
    saveRDS(mat1,file = sprintf("output/RDS/%s_%s_Buffer%s_Year%s_Hansen", Countrycode, Chronosequence, BufferDistance, Year))
    write.xlsx(mat2, file = sprintf("output/Excel/%s_%s_Buffer%s_Year%s.xlsx", Countrycode, Chronosequence, BufferDistance, Year), sheetName = "Kim", append = T)
    saveRDS(mat2,file = sprintf("output/RDS/%s_%s_Buffer%s_Year%s_Kim", Countrycode, Chronosequence, BufferDistance, Year))
    
  } else if (Year == 2005){ 
    write.xlsx(mat4, file = sprintf("output/Excel/%s_%s_Buffer%s_Year%s.xlsx", Countrycode, Chronosequence, BufferDistance, Year), sheetName = "Sexton")
    saveRDS(mat4,file = sprintf("output/RDS/%s_%s_Buffer%s_Year%s_Sexton", Countrycode, Chronosequence, BufferDistance, Year))
    write.xlsx(mat5, file = sprintf("output/Excel/%s_%s_Buffer%s_Year%s.xlsx", Countrycode, Chronosequence, BufferDistance, Year), sheetName = "Kim", append = T)
    saveRDS(mat5,file = sprintf("output/RDS/%s_%s_Buffer%s_Year%s_Kim", Countrycode, Chronosequence, BufferDistance, Year))
    
  } else if (Year == 2012) {
    write.xlsx(mat6, file = sprintf("output/Excel/%s_%s_Buffer%s_Year%s.xlsx", Countrycode, Chronosequence, BufferDistance, Year), sheetName = "Hansen")
    saveRDS(mat6,file = sprintf("output/RDS/%s_%s_Buffer%s_Year%s_Hansen", Countrycode, Chronosequence, BufferDistance, Year))
  } else {
    print("cant write to excel or to RDS")
  }
}
