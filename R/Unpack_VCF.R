# Unpack VCF data

Unpack_VCF <- function(pr_filename, x, extract, Year, pr, dir){
  if (pr_filename %in% x){
    print("File allready exsist, checking if size == 0")
    if (file.info(sprintf('%s/%s%s', dir, extract, pr_filename))$size == 0){
      unlink(sprintf('%s/%s%s',dir, extract, pr_filename))
      unpackVCF(pr=pr, year = Year, searchDir=dir, dir=sprintf('%s/%s',dir, extract))
    } else {
      print("File allready exsist, no need for unpacking")
    }
  } else {
    unpackVCF(pr=pr, year = Year, searchDir=dir, dir=sprintf('%s/%s',dir, extract))
  }
}