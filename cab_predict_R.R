#remove all the objects stored
rm(list=ls())

#set current working directory
setwd("F:/analytics_basics")

# Load CSV
train_df = read.csv("train_cab.csv", header = TRUE)

# Data exploration and cleaning

train_df$fare_amount = as.numeric(as.character(train_df$fare_amount))
summary(train_df)
#replace 0's in coordinates with null values
train_df[train_df == 0] <- NA
#now removing these rows with null values
train_df<-na.omit(train_df)

#removing the values less than 0 and greater than 99.99 percentile
train_df = subset(train_df, (train_df$fare_amount > 0 ) & (train_df$fare_amount < quantile(train_df$fare_amount,.9999)))
#only accepting rows with values greter thn or equal to 1 or less than 7.
train_df = subset(train_df, (train_df$passenger_count >=1)  &(train_df$passenger_count < 7) )

#removing the values less than 0 and greater than 99.99 percentile
coords = c("pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude")
for (i in coords){ 
  train_df = subset(train_df, (train_df[,i] < quantile(train_df[,i],.999)))
}

#adding new variables to data
train_df$diff_longitude= abs(train_df$dropoff_longitude - train_df$pickup_longitude)
train_df$diff_latitude= abs(train_df$dropoff_latitude - train_df$pickup_latitude) 

train_df = subset(train_df,(train_df$diff_longitude < 5.0) & (train_df$diff_latitude < 5.0))

#getting distance between pickup and dropoff location using haversine function

getDistanceFromLatLonInKm <- function(lat1,lon1,lat2,lon2) {
  R = 6371 #radius of earth
  dLat = deg2rad(lat2-lat1)
  dLon = deg2rad(lon2-lon1)
  a = sin(dLat/2) * sin(dLat/2) + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin(dLon/2) * sin(dLon/2)
  
  c = 2 * atan2(sqrt(a), sqrt(1-a))
  d = R * c
  return (d)
}

deg2rad <- function(deg)
{
  PI=22/7
  
  return (deg * (PI/180))
}

#removing rows with distance equal to zero
train_df$distance = getDistanceFromLatLonInKm(train_df$pickup_longitude,train_df$pickup_latitude,train_df$dropoff_longitude,train_df$dropoff_latitude)
train_df = subset(train_df, (train_df$distance > 0 ))

#Machine Learning

# Bin the fare and convert to string
train_df$fare_bin = cut(train_df$fare_amount, seq(from = 0, to = 80, by = 5))
library(caTools)
train_rows = sample.split(train_df$fare_bin, SplitRatio=0.8)
Sample1 = train_df[ train_rows,]
Rest1  = train_df[!train_rows,]

#######################################   Linear Regression #############################################################
#run regression model
lm_model = lm(fare_amount ~ passenger_count  + diff_longitude + diff_latitude , data = Sample1)
#Predict
predictions_LR = predict(lm_model, Rest1[,7:9])
#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y)*100)
}
MAPE(Rest1[,1], predictions_LR)

#Calculate RMSE
RMSE = function(y_true, y_pred){
  sqrt(mean((y_true-y_pred)**2))
}
RMSE(Rest1[,1], predictions_LR)

#######################################   Decision Tree #############################################################
library(rpart)
library(MASS)

# ##rpart for regression
dt_model = rpart(fare_amount ~ passenger_count  + diff_longitude + diff_latitude , data = Sample1, method = "anova")

#Predict for new test cases
predictions_DT = predict(dt_model, Rest1[,7:9])
#calculating error metrics
MAPE(Rest1[,1], predictions_DT)
RMSE(Rest1[,1], predictions_DT)

#######################################   Random Forest #############################################################
#install.packages("randomForest")
library(randomForest)
# Create a Random Forest model with default parameters
rf_model <- randomForest(fare_amount ~ passenger_count  + diff_longitude + diff_latitude , data = Sample1, importance = TRUE,ntree = 200)
#Predict for new test cases
predictions_RF = predict(rf_model, Rest1[,7:9])
MAPE(Rest1[,1], predictions_RF)
RMSE(Rest1[,1], predictions_RF)

#######################################   KNN Regression #############################################################
KNN_predictions = FNN::knn.reg(train = Sample1[,7:9], test = Rest1[,7:9], y = Sample1$fare_amount, k = 111)
MAPE(Rest1[,1], KNN_predictions$pred)
RMSE(Rest1[,1], KNN_predictions$pred)
#########################################################################################################################
# Load CSV
test = read.csv("test.csv", header = TRUE)
#adding diff_lat/long to test
test$diff_longitude= abs(test$dropoff_longitude - test$pickup_longitude)
test$diff_latitude= abs(test$dropoff_latitude - test$pickup_latitude) 
KNN_predictions2 = FNN::knn.reg(train = Sample1[,7:9], test = test[,6:8], y = Sample1$fare_amount, k = 111)
test$fare_amount = KNN_predictions2$pred
write.csv(test, "test_new_R.csv", row.names = F)