#Module 2
#Predicting Brand Preference

library(readr)
library(ggplot2)
library(dplyr)
library(lattice)
library(caret)
library(tidyr)

#Module 2

response<-read.csv2("Data/surveyBelkinElago.csv")
response

incomplete <- subset(response, brand == " ")

complete <- subset(response, brand !=" ")


#convert [zipcode, car, elevel] into factors
complete <- complete %>% 
  mutate_at(c("elevel", "zipcode", "car", "brand"), factor)


#splitting the data
set.seed(108)
inTrain <- createDataPartition(complete$brand, 
                               times = 1,
                               p = 0.7,
                               list = FALSE)

#?createDataPartition

completeTrain <- complete[inTrain, ]
completeTest <- complete[-inTrain, ]

#scaling [columns salary, age] 

#?preProcess
#choosing features salary and age as predictors

numerics <- c("salary" , "age")

#run the preprocess function on  salary and age features to scale the data
ppvalues <- preProcess(complete[, numerics], 
                       method = c("center", "scale"),
                       verbose = FALSE) 

head(ppvalues)
#NOTE: preProcess estimates mean and sd of the preprocess values but doesn't actually do the transformation

#Use the preprocess method to scale, center the training and testing data sets
proctrain <- predict(ppvalues,completeTrain)
proctest <- predict(ppvalues, completeTest)
incomplete <- predict(ppvalues, incomplete)
incomplete
View(proctrain)
?trainControl

#Building the model

#Defining how we would like the model  built
fitting <- trainControl(method = "cv",
                        number = 10,
                        search = "grid",
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        savePredictions = "final")
str(proctrain)
#Random Forest Model
#?train

set.seed(108)
RFmodel <- train(brand~salary+age, 
            data = proctrain,
            method = "ranger",
            trControl = fitting,
            tuneLength = 2, metric = "ROC")
RFmodel
summary(RFmodel)

#?predict
rfresults <- predict(RFmodel, incomplete)
rfresults

postResample(rfresults, proctest$brand)
confusionMatrix(rfresults, proctest$brand)

set.seed(108)
C5Model <- train(x = proctrain[,numerics],
                 y = proctrain$brand,
                 method = "C5.0",
                 trControl = fitting,
                 tunelength = 2)

c5results <- predict(C5Model, incomplete)

?postResample
postResample(c5results, proctest$brand)

