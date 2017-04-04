
# A function that reads, cleans and outputs past data to be visualized
readPastDataset <- function(filePath, sep = "\t", date_from = "2016/03/04", date_till = "2016/03/26") {
  raw <- read.csv(filePath, sep = sep)

  # Clean and subset raw input 
  raw$Date <- as.Date(mydata$Date)
  pastData <- subset(raw, Date %<%  date_till & date_from %<%  Date)
  pastData[complete.cases(pastData), ]
}



