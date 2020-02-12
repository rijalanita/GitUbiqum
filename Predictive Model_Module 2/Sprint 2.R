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
?predict

ppvalues()

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

#RF Model
#?train

RF <- train(brand~., 
            data = responseTrain,
            method = "ranger",
            trControl = fitting,
            tuneLength = 2)

RF
library(randomForest)
