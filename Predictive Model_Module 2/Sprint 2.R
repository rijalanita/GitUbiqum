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

#Building the model

fitting <- trainControl(method = "cv",
                        number = 10,
                        search = "grid",
                        #classProbs = TRUE,
                        #summaryFunction = twoClassSummary,
                        savePredictions = "final")

set.seed(108)
RFmodel <- train(brand~salary+age, 
            data = proctrain,
            method = "ranger",
            trControl = fitting,
            tuneLength = 2)
            #metric = "ROC")
RFmodel
summary(RFmodel)

#?predict
rfresults <- predict(RFmodel, incomplete)
rfresults

#accuracy of RF model
?postResample
postResample(pred = rfresults, obs = proctest$brand)
accuracy_RF<-confusionMatrix(RFmodel, newdata = proctest$brand)
accuracy_RF

set.seed(108)
C5Model <- train(x = proctrain[,numerics],
                 y = proctrain$brand,
                 method = "C5.0",
                 trControl = fitting,
                 tunelength = 2)
C5Model

c5results <- predict(C5Model, incomplete)
c5results

#accuracy of c5.0 model
postResample(pred = c5results, obs = proctest$brand)
c5_accuracy <- confusionMatrix(C5Model, newdata= proctest)
c5_accuracy

#comparing the 2 different model performances
remodel<- resamples(list(rf=RFmodel, c5=C5Model))
summary(remodel)

#Report for predictions using c50 Model

mypredictions <- cbind(incomplete, "predictions" = c5results)
mypredictions <- within(incomplete, rm(brand))
mypredictions

visual<-ggplot(c5table, aes(x=brand, y =, fill=brand))
visual+geom_bar()
