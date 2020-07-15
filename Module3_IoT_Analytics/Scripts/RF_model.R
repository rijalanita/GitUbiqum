# Title: Random Forest Model
#Version: 1.0
#Author: Anita Rijal
#Date: March 27, 2020


library(caret)
library(dplyr)

#load df object
train_set<- readRDS('./train_set')
validation_set <- readRDS('./validation_set')

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

#Creating df with just predictors and target value
Trainset<- Trainset %>% 
  select(starts_with("WAP"), locationID)

Testset<- Testset %>% 
  select(starts_with("WAP"), locationID)

#RANDOM FOREST CLASSIFIER MODEL
set.seed(108)
RFmodel <- train(factor(locationID) ~., 
                 data = Trainset,
                 trControl = fitting,
                 tuneLength = 2)

#predicting test set outcomes using training set
testresults <- predict(RFmodel, Testset)

#using validation data set to predict
modelresults <- predict(RFmodel,validate_set)

confusionMatrix(testresults, Testset$locationID)
confusionMatrix(modelresults, validate_set$locationID)



