source('read_past_data.R')

# To Do: produce a real prediction matrix
predict <- function(training_set = NULL) {
  fakePredictions = "data/2016_pred.csv"
  predictions <-  read.csv(fakePredictions)
  predictions$Date <- gsub( " .*$", "", predictions$Date )
  predictions$Date <- as.Date(predictions$Date, format = "%m/%d/%Y")
  
  
  date_from <-"2016/03/04"
  today <- as.Date("2016/03/19")
  date_till <-"2016/03/26"
  
  predictions <- subset(predictions,( today  %<% Date)  & (Date  %<% date_till), select = c(Latitude, Longitude, Date))
  predictions[['pred']] <- rep(T, nrow(predictions)) 
  predictions
}
