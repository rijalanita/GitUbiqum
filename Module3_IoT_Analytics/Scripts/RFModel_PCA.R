#Title:       PCA-MODELING Feasibility of 'Wifi Fingerprinting' to Determine Person's Indoor Location
#Version:     1.0
#Date:        March 27, 2020

library(dplyr)
library(tidyverse)
library(caret)

pca_train <- readRDS("pca_train")
pca_validation <- readRDS("pca_validation")
train_set <- readRDS("train_clean")
validate_set <- readRDS("validation_clean")


pca_train_RF <- pca_train %>% 
  mutate(location = as.factor(train_set$locationID))

pca_val_RF <- pca_validation %>% 
  mutate(location = as.factor(validate_set$locationID))

#splitting the data
set.seed(108)
inTrain_rf <- createDataPartition(pca_train_RF$location, 
                               times = 1,
                               p = 0.7,
                               list = FALSE)

Trainset_rf <- pca_train_RF[inTrain_rf, ]

Testset_rf <- pca_train_RF[-inTrain_rf, ]


fitting <- trainControl(method = "cv",
                        number = 10,
                        search = "grid",
                        savePredictions = "final")

#RANDOM FOREST CLASSIFIER MODEL
set.seed(108)
RFmodel <- train(factor(location) ~., 
                 data = Trainset_rf,
                 method = "ranger",
                 trControl = fitting,
                 tuneLength = 2)
RFmodel
summary(RFmodel)

#predicting test set outcomes using training set
testresults <- predict(RFmodel, Testset_rf)

#using validation data set to predict
modelresults <- predict(RFmodel,pca_val_RF)

confusionMatrix(testresults, Testset_rf$location)
confusionMatrix(modelresults, pca_val_RF$location)

