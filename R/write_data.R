Write_fun <- function(matrix_list){
  dir.create(file.path('output/Excel'), showWarnings = FALSE)
  dir.create(file.path('output/RDS'), showWarnings = FALSE)
  j <- 1
  for (i in matrix_list){
    name_sheet <- names(matrix_list[j])
    write.xlsx(matrix_list[[j]], file = sprintf("output/Excel/Buffer%s_Threshold%s_Year%s.xlsx", BufferDistance, Threshold, Year), sheetName = names(matrix_list[j]), append = T)
    saveRDS(matrix_list[j],file = sprintf("output/RDS/Buffer%s_Threshold%s_%s", BufferDistance, Threshold, name_sheet))
    j <- j + 1
  }
  
}