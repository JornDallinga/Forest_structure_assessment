Write_chrono_excel <- function(matrix_list){
  dir.create(file.path('output/Excel'), showWarnings = FALSE)
  dir.create(file.path('output/Excel/Chronosequences'), showWarnings = FALSE)
  
  ## Find unique chronosequences
  unique_chrono <- unique(matrix_list[[1]]$Chronosequence)
  j <- 1
  
  for (i in 1:length(unique_chrono)){
    
    ## subset each unique chronosequence
    write_excel <- lapply(matrix_list, function(x)x[x$Chronosequence == unique_chrono[j],])
    
    ## create a directory of each unique chronosequence
    dir.create(file.path(sprintf('output/Excel/Chronosequences/%s', unique_chrono[j])), showWarnings = FALSE)
    
    ## write excel file of each individual chronosequence (Using XLConnect because it overwrites the excel file instead of causing an error)
    #lapply(1:length(names(write_excel)), function(i) writeWorksheetToFile(data = write_excel[[i]], file = sprintf("output/Excel/Chronosequences/%s/Buffer%s_Threshold%s_Year%s.xlsx", unique_chrono[j], BufferDistance, Threshold, Year), sheet = names(write_excel[i])))
    lapply(1:length(names(write_excel)), function(i) write.xlsx(write_excel[[i]], file = sprintf("output/Excel/Chronosequences/%s/Buffer%s_Threshold%s_Year%s.xlsx", unique_chrono[j], BufferDistance, Threshold, Year), sheetName = names(write_excel[i]), append = T))
    j <- j + 1
  }

}
