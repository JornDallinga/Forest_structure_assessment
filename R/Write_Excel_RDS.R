Write_fun <- function(Year){
  if (Year == 1990){
    write.xlsx(mat3, file = sprintf("output/%s_buffer%s_year%s.xlsx", Countrycode, BufferDistance, Year), sheetName = "Kim")
    saveRDS(mat3,file = sprintf("output/%s_buffer%s_year%s_Kim", Countrycode, BufferDistance, Year))
    
  } else if (Year == 2000){ 
    write.xlsx(mat, file = sprintf("output/%s_buffer%s_year%s.xlsx", Countrycode, BufferDistance, Year), sheetName = "Sexton")
    saveRDS(mat,file = sprintf("output/%s_buffer%s_year%s_Sexton", Countrycode, BufferDistance, Year))
    write.xlsx(mat1, file = sprintf("output/%s_buffer%s_year%s.xlsx", Countrycode, BufferDistance, Year), sheetName = "Hansen", append = T)
    saveRDS(mat1,file = sprintf("output/%s_buffer%s_year%s_Hansen", Countrycode, BufferDistance, Year))
    write.xlsx(mat2, file = sprintf("output/%s_buffer%s_year%s.xlsx", Countrycode, BufferDistance, Year), sheetName = "Kim", append = T)
    saveRDS(mat2,file = sprintf("output/%s_buffer%s_year%s_Kim", Countrycode, BufferDistance, Year))
    
  } else if (Year == 2005){ 
    write.xlsx(mat4, file = sprintf("output/%s_buffer%s_year%s.xlsx", Countrycode, BufferDistance, Year), sheetName = "Sexton")
    saveRDS(mat4,file = sprintf("output/%s_buffer%s_year%s_Sexton", Countrycode, BufferDistance, Year))
    write.xlsx(mat5, file = sprintf("output/%s_buffer%s_year%s.xlsx", Countrycode, BufferDistance, Year), sheetName = "Kim", append = T)
    saveRDS(mat5,file = sprintf("output/%s_buffer%s_year%s_Kim", Countrycode, BufferDistance, Year))
    
  } else if (Year == 2012) {
    write.xlsx(mat6, file = sprintf("output/%s_buffer%s_year%s.xlsx", Countrycode, BufferDistance, Year), sheetName = "Hansen", append = T)
    saveRDS(mat6,file = sprintf("output/%s_buffer%s_year%s_Hansen", Countrycode, BufferDistance, Year))
  } else {
    print("cant write to excel or to RDS")
  }
}
