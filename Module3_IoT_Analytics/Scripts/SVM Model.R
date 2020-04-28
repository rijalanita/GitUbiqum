#Title:       PCA-MODELING Feasibility of 'Wifi Fingerprinting' to Determine Person's Indoor Location
#Version:     1.0
#Date:        April 15, 2020

library(dplyr)
library(tidyverse)
library(caret)

pca_train <- readRDS("pca_train")
pca_validation <- readRDS("pca_validation")
train_set <- readRDS("train_clean")
validate_set <- readRDS("validation_clean")

pca_train_svm <- pca_train %>% 
                mutate(location = as.factor(train_set$locationID))

pca_val_svm <- pca_validation %>% 
  mutate(location = as.factor(validate_set$locationID))

set.seed(108)
inTrain_svm <- createDataPartition(pca_train_svm$location, 
                               times = 1,
                               p = 0.7,
                               list = FALSE)

Trainset_svm <- pca_train_svm[inTrain_svm, ]

Testset_svm <- pca_train_svm[-inTrain_svm, ]

#SVM model

fitting_svm <- trainControl(method = "repeatedcv", 
                            number = 10, 
                            repeats = 3)
set.seed(108)
svm_Linear <- train(location ~., 
                    data = pca_train_svm, 
                    method = "svmLinear",
                    trControl=fitting_svm,
                    tuneLength = 10)

svm_Linear
summary(svm_Linear)

test_svm <- predict(svm_Linear, Testset_svm)
svm_results <- predict(svm_Linear, pca_val_svm)

confusionMatrix(test_svm, Testset_svm$location)
confusionMatrix(svm_results, pca_val_svm$location)
str(Testset_svm$location)
