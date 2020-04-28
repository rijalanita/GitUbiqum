#Title:       Feasibility of 'Wifi Fingerprinting' to Determine Person's Indoor Location
#Version:     1.0
#Date:        March 27, 2020

library(readr)
library(lubridate)
library(dplyr)
library(tidyverse)
library(caret)

trainingData <- read_csv("~/Desktop/Ubiqum/GitUbiqum/IoT Analytics/Data/trainingData.csv")
View(trainingData)

validationData <- read_csv("~/Desktop/Ubiqum/GitUbiqum/Iot Analytics/Data/validationData.csv")


#reordering the columns of dataframe
trainingData_clean<- trainingData[, c(521:529, 1:520)]    

validationData_clean <- validationData[, c(521:529,1:520)]

#save as file R object
saveRDS(trainingData_clean, file = "")

#investigating data structure
summary(trainingData_clean$USERID)
dim(trainingData_clean)
str(trainingData_clean)
colnames(trainingData_clean)
trainingData_clean$PHONEID


#visualising the data

boxplot(trainingData_clean$LONGITUDE, main = "Boxplot of Longitudes", ylab = "Longitude")
boxplot(trainingData_clean$LATITUDE, main = "Boxplot of Latitiudes", ylab = "LATITUDE")

?hist
hist(trainingData_clean$USERID, xlab = "USERID", breaks = 18)
hist(trainingData_clean$PHONEID, breaks = 20)

plot(trainingData_clean$TIMESTAMP, trainingData_clean$BUILDINGID)
table(trainingData_clean$USERID)
table((trainingData_clean$BUILDINGID))

table(trainingData_clean$WAP500)
table(trainingData_clean$SPACEID)


#converting data types into factors
trainingData_clean %>% 
  mutate_at(vars(, c(3:8)), as.factor)

#Converting UNIX date into datetime
trainingData_clean$TIMESTAMP <-as_datetime(trainingData$TIMESTAMP)


#SAMPLING 1000 rows
#splitting the data
set.seed(108)
inTrain <- createDataPartition(trainingData_clean$BUILDINGID, 
                               times = 1,
                               p = 0.7,
                               list = FALSE)

completeTrain <- trainingData_clean[inTrain, ]

completeTest <- trainingData_clean[-inTrain, ]

