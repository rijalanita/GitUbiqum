#Module 2
#Predicting Brand Preference

library(readr)
library(ggplot2)
library(dplyr)
library(lattice)
library(caret)

#Module 2
#Predicting Brand Preference
response <- read_csv("Data/CompleteResponses.csv")
summary(response)

#convert [zipcode, car, elevel] into factors
response <- response %>% 
  mutate_at(c("elevel", "zipcode", "car", "brand"), factor)

#scaling [columns salary, age] 

#?preProcess
numerics <- c("salary" , "age" , "credit")
ppvalues <- preProcess(response[, numerics], 
                       method = c("center", "scale"),
                       verbose = FALSE) 

ppvalues

#splitting the data
set.seed(108)
inTrain <- createDataPartition(response$brand, 
                               times = 1,
                               p = 0.7,
                               list = FALSE)

#?createDataPartition

responseTrain <- response[inTrain, ]
responseTest <- response[-inTrain, ]

?trainControl

#Building the model

#Defining how we would like the model  built
fitting <- trainControl(method = "cv",
                        number = 10,
                        search = "grid")
responseTrain$brand<-as.factor(responseTrain$brand)

#modelLookup("rf")
#RF Model
#?train

RF <- train(brand~., 
            data = responseTrain,
            method = "rf",
            trControl = "fitting",
            tuneGrid = "grid",
            tuneLength = 2)


#rforest <- train(response ~ ., data = data_set,
method = "rf",
ntree = 1000,
trControl = ctrl,
tuneGrid = data.frame(mtry = 6))

str(responseTrain)

