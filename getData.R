##=============================================================
## Function: getData
## Args:
##   None  
## Returns:
##    a dataframe 
##
## Purpose:
##   A function that checks for zip file, extracts the zip file,
##=============================================================
getData <- function(){
  
  #  vars for files
  fileName <- "repdata-data-activity.zip"
  datafile <-"activity.csv"
  
  # download file if necessary
  if(! file.exists(fileName)) {
    message(paste("Downloading ", fileName ," ..."))
    fileURL="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url=fileURL,destfile=fileName)
  }
  
  # extract if needed
  if(! file.exists(datafile)) {
    message(paste("Extracting ", filename, " from the zip file ..."))
    unzip(zipfile=fileName)
  }
  else {
    message("Data file already extracted ...")
  }
  
  # file is csv 
   message("Loading actvity data for October and November in 2012 ...")
  df <- read.csv (datafile,
                            header = TRUE,
                            sep = ","
                           )

  message("Returning a data frame ...")
return (df)
}