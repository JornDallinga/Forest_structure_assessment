# Unpack VCF data

Unpack_VCF <- function(pr_filename, x, extract, Year, pr, dir){
  t = 1
  
  for (i in x){
    if (pr_filename[t] %in% x){
      if (file.exists(sprintf('%s/%s%s', dir, extract, pr_filename[t]))){
        print("File allready exsist, checking if file == 0 bytes")
        if (file.info(sprintf('%s/%s%s', dir, extract, pr_filename[t]))$size == 0){
          unlink(sprintf('%s/%s%s',dir, extract, pr_filename[t]))
          unpackVCF(pr=pr, year = Year, searchDir=dir, dir=sprintf('%s/%s',dir, extract))
        } else {
        }
      } else {
        unpackVCF(pr=pr, year = Year, searchDir=dir, dir=sprintf('%s/%s',dir, extract))
      }

    } else {
      unpackVCF(pr=pr, year = Year, searchDir=dir, dir=sprintf('%s/%s',dir, extract))
    }
  t <- t + 1
  }
}
