
# Operator overloading for date comparison
'%<%' <- function(date1, date2) {
  as.numeric(as.POSIXct(date1))  <= as.numeric(as.POSIXct(as.Date(date2)))
}

# A function that reads, cleans and outputs past data to be visualized
readPastDataset <- function(filePath, sep = "\t", date_from = "2016/03/04", date_till = "2016/03/26") {
  raw <- read.csv(filePath, sep = sep)

  # Clean and subset raw input 
  raw$Date <- as.Date(raw$Date)
  pastData <- subset(raw, Date %<%  date_till & date_from %<%  Date)
  pastData[complete.cases(pastData), ]
}



