#Title:       Wifi Locationing (Preprocessing)
#Version:     1.0
#Date:        March 27, 2020

library(readr)
library(lubridate)
library(dplyr)
library(tidyverse)
library(caret)

#Load csv file
trainingData <- read_csv("./trainingData.csv")
validationData <- read_csv("./validationData.csv")

#reordering the columns of dataframe
trainingData<- trainingData[, c(521:529, 1:520)]    
validationData <- validationData[, c(521:529,1:520)]

#replace 100 (no wifi signal indicator) values with -105
trainingData<- trainingData %>% 
  select( c(1:10), starts_with("WAP")) %>% 
  mutate_all(funs(ifelse(.==100, -105, .)))

validationData<- validationData %>% 
  select(c(1:10), starts_with("WAP")) %>% 
  mutate_all(funs(ifelse(.==100, -105, .)))

#converting data types into factors
trainingData <- trainingData %>% 
  mutate_at(vars(, c(3:8)), as.factor)

validationData <- validationData %>% 
  mutate_at(vars(, c(3:8)), as.factor)

#Converting UNIX date into datetime
trainingData$TIMESTAMP <-as_datetime(trainingData$TIMESTAMP)
validationData$TIMESTAMP <- as_datetime(validationData$TIMESTAMP)

#Create new column LocationID by merging BuidlingID and FloorID for classifier model
trainingData<- trainingData %>% 
  unite("locationID", c(BUILDINGID,FLOOR), sep ="", remove = F)

validationData <- validationData %>% 
  unite("locationID", c(BUILDINGID, FLOOR), sep ="", remove = F)

#Reducing size of df by removing low variance WAPs
remove_cols <- nearZeroVar(trainingData, names = T, freqCut = 1000, uniqueCut = 0.5)

trainingData <- trainingData %>%
  select(, -all_of(remove_cols))

validationData<- validationData %>%
  select(, -all_of(remove_cols))

saveRDS(trainingData, file = 'train_clean')
saveRDS(validationData, file = 'validation_clean')


