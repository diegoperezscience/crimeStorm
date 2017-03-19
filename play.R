setwd('D:/Xomnia Hackathon/code')
mydata <- read.csv("Crimes_-_2001_to_present.csv", nrows=2000)  # read csv file 
mydata <- mydata[complete.cases(mydata),]

newdata <- subset(mydata, Latitude >= bounds_coords[['south']] & Latitude <= bounds_coords[['north']] &
                    Longitude <= bounds_coords[['east']] & Longitude >= bounds_coords[['west']])

mydata$Longitude 
mydata$Latitude