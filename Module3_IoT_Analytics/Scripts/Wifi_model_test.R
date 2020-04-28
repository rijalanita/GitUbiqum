#Title:       MODELING Feasibility of 'Wifi Fingerprinting' to Determine Person's Indoor Location
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
train_set <- readRDS("train_clean")
validate_set <- readRDS("validation_clean")

#change LOCATIONID type to factor
train_set$locationID<- as.factor(train_set$locationID)
validate_set$locationID <- as.factor(validate_set$locationID)

#SAMPLING 1000 rows
set.seed(108)
training1000 <- stratified(train_set, group = "BUILDINGID", size = 333)

#splitting the data
set.seed(108)
inTrain <- createDataPartition(training1000$locationID, 
                               times = 1,
                               p = 0.7,
                               list = FALSE)

Trainset <- training1000[inTrain, ]

Testset <- training1000[-inTrain, ]


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
RFmodel
summary(RFmodel)

#predicting test set outcomes using training set
testresults <- predict(RFmodel, Testset)

#using validation data set to predict
modelresults <- predict(RFmodel,validate_set)

confusionMatrix(testresults, Testset$locationID)
confusionMatrix(modelresults, validate_set$locationID)

#SVM model

fitting_svm <- trainControl(method = "repeatedcv", 
                            number = 10, 
                            repeats = 3)
set.seed(108)
svm_Linear <- train(locationID ~., 
                    data = Trainset, 
                    method = "svmLinear",
                    trControl=fitting_svm,
                    tuneLength = 10)

svm_Linear
summary(svm_Linear)

test_svm <- predict(svm_Linear, Testset)
svm_results <- predict(svm_Linear, validate_set)

confusionMatrix(test_svm, Testset$locationID)
confusionMatrix(svm_results, validate_set$locationID)

#GRADIENT BOOSTED REGRESSION MODEL
fitting_gbm <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 10)

Trainset_lat <- training1000[inTrain, ]

Testset_lat <- training1000[-inTrain, ]

Trainset_lat<- Trainset_lat %>% 
  select(starts_with("WAP"), LATITUDE)

Testset_lat<- Testset_lat %>% 
  select(starts_with("WAP"), LATITUDE)


gbm_regression <- train(LATITUDE ~ ., 
                        data = Trainset_lat, 
                        method = "gbm", 
                        trControl = fitting_gbm,
                        verbose = FALSE)

gbm_regression

test_gbm <- predict(gbm_regression, Testset$LATITUDE)
gbm_results <- predict(gbm_regression, validate_set)

postResample(gbm_results, validate_set$LATITUDE)


#USING LONGITUDE WITH GBM MODEL
Trainset_long <- training1000[inTrain, ]
Testset_long <- training1000[-inTrain, ]


Trainset_long<- Trainset_long %>% 
  select(starts_with("WAP"), LONGITUDE)

Testset_long<- Testset_long %>% 
  select(starts_with("WAP"), LONGITUDE)


gbm_regression <- train(LONGITUDE ~ ., 
                        data = Trainset_long, 
                        method = "gbm", 
                        trControl = fitting_gbm,
                        verbose = FALSE)

GBM_testlong <- predict(gbm_regression, Testset_long)

GBM_resultlong <- predict(gbm_regression, validate_set)

postResample(GBM_resultlong, validate_set$LONGITUDE)
