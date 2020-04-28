#Title:       PREPROCESSING: Feasibility of 'Wifi Fingerprinting' to Determine Person's Indoor Location
#Version:     1.0
#Date:        March 27, 2020


library(readr)
library(lubridate)
library(dplyr)
library(tidyverse)
library(caret)

#read R object files
train_clean<-readRDS("train_clean")
validation_clean<-readRDS("validation_clean")

#converting data types into factors
train_clean <- train_clean %>% 
  mutate_at(vars(, c(3:8)), as.factor)

validation_clean <- validation_clean %>% 
  mutate_at(vars(, c(3:8)), as.factor)

#Converting UNIX date into datetime
train_clean$TIMESTAMP <-as_datetime(train_clean$TIMESTAMP)
validation_clean$TIMESTAMP <- as_datetime(validation_clean$TIMESTAMP)

#merge columns
train_clean<- train_clean %>% 
 unite("locationID", c(BUILDINGID,FLOOR), sep ="", remove = F)

validation_clean <- validation_clean %>% 
  unite("locationID", c(BUILDINGID, FLOOR), sep ="", remove = F)

#remove low variance columns
remove_cols <- nearZeroVar(train_clean, names = T, freqCut = 2500, uniqueCut = 0.5)

train_clean <- train_clean %>%
  select(, -all_of(remove_cols))

validation_clean <- validation_clean %>%
  select(, -all_of(remove_cols))

#save dataframe as R object
saveRDS(train_clean, file = "train_clean")
saveRDS(validation_clean, file = "validation_clean")
 

