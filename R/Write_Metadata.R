Write_metadata <- function(mat, Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold){
  mat[i, count] <- paste(Countrycode)
  count <- count + 1
  mat[i, count] <- paste(Chronosequence)
  count <- count + 1
  mat[i, count] <- paste(Year)
  count <- count + 1
  mat[i, count] <- paste(BufferDistance)
  count <- count + 1
  mat[i, count] <- paste(Threshold)
  count <- count + 1
  mat[i, count] <- mydata$x[1 + j]
  count <- count + 1
  mat[i, count] <- mydata$y[1 + j]
  count <- count - 6
  return(mat)
  
}