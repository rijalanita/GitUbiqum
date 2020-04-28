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

#ADD LATITUDE
pca_train <- pca_train %>% 
              mutate(lat = train_set$LATITUDE)

pca_validation <- pca_validation %>% 
                  mutate(lat = validate_set$LATITUDE)

#GRADIENT BOOSTED REGRESSION MODEL

#splitting the data
set.seed(108)
inTrain <- createDataPartition(pca_train$lat, 
                               times = 1,
                               p = 0.7,
                               list = FALSE)

Trainset_lat <- pca_train[inTrain, ]
Testset_lat <- pca_train[-inTrain, ]

fitting_gbm <- trainControl(method = "repeatedcv",
                            number = 5,
                            repeats = 10)


gbm_regression <- train(lat ~., 
                        data = Trainset_lat, 
                        method = "gbm", 
                        trControl = fitting_gbm,
                        verbose = FALSE)

gbm_regression

test_gbm <- predict(gbm_regression, Testset_lat)
gbm_results <- predict(gbm_regression, pca_validation)

postResample(gbm_results, pca_validation$lat)


#USING LONGITUDE WITH GBM MODEL
train_long <- pca_train %>% 
  mutate(long = train_set$LONGITUDE)

validate_long <- pca_validation %>% 
  mutate(long = validate_set$LONGITUDE)

set.seed(108)
inTrain2 <- createDataPartition(train_long$long, 
                               times = 1,
                               p = 0.7,
                               list = FALSE)

Trainset_long <- train_long[inTrain, ]
Testset_long <- train_long[-inTrain, ]

gbm_regression <- train(long ~ ., 
                        data = Trainset_long, 
                        method = "gbm", 
                        trControl = fitting_gbm,
                        verbose = FALSE)

saveRDS(gbm_regression, file = "GBM")

GBM_testlong <- predict(gbm_regression, Testset_long)
GBM_resultlong <- predict(gbm_regression, validate_long)

postResample(GBM_resultlong, validate_long$long)

names(pca_train)

