#Title:       Feasibility of 'Wifi Fingerprinting' to Determine Person's Indoor Location
#Version:     1.0
#Date:        March 27, 2020

library(readr)
library(lubridate)
library(dplyr)
library(tidyverse)
library(caret)
library(sampler)
library(splitstackshape)


#read saved R object
train_set <- readRDS("train_distinct")
validate_set <- readRDS("validation_clean")

#change LOCATIONID type to factor
train_set$locationID<- as.factor(train_set$locationID)
validate_set$locationID <- as.factor(validate_set$locationID)

#SAMPLING 1000 rows
set.seed(108)
training1000 <- stratified(train_set, group = "BUILDINGID", size = 0.33)

set.seed(108)
inTrain <- createDataPartition(train_set$locationID, 
                               times = 1,
                               p = 0.7,
                               list = FALSE)

Trainset <- train_set[inTrain, ]

Testset <- train_set[-inTrain, ]


fitting <- trainControl(method = "cv",
                        number = 10,
                        search = "grid",
                        savePredictions = "final")


Trainset<- Trainset %>% 
  select(starts_with("WAP"), locationID)

Testset<- Testset %>% 
  select(starts_with("WAP"), locationID)

#RANDOM FOREST CLASSIFIER MODEL
set.seed(108)

RFmodel <- train(factor(locationID) ~., 
                 data = Trainset,
                 method = "ranger",
                 trControl = fitting,
                 tuneLength = 2)
#predicting test set outcomes using training set
testresults <- predict(RFmodel, Testset)

#using validation data set to predict
modelresults <- predict(RFmodel,validate_set)

confusionMatrix(testresults, Testset$locationID)
confusionMatrix(modelresults, validate_set$locationID)

