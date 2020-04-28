#Title:       Feasibility of 'Wifi Fingerprinting' to Determine Person's Indoor Location
#Version:     1.0
#Date:        March 27, 2020

library(readr)
library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)

trainingData <- read_csv("~/Desktop/Ubiqum/GitUbiqum/IoT Analytics/Data/trainingData.csv")
View(trainingData)

validationData <- read_csv("~/Desktop/Ubiqum/GitUbiqum/Iot Analytics/Data/validationData.csv")


#reordering the columns of dataframe
trainingData<- trainingData[, c(521:529,1:520)]    

validationData <- validationData[, c(521:529,1:520)]
          
#investigating data structure
names(train_clean)
head(train_clean, n=100)

train_clean <- trainingData
validation_clean <- validationData


#replace 100 values with -105
train_clean<- train_clean %>% 
  select( c(1:10), starts_with("WAP")) %>% 
  mutate_all(funs(ifelse(.==100, -105, .)))

validation_clean<- validation_clean %>% 
  select(c(1:10), starts_with("WAP")) %>% 
  mutate_all(funs(ifelse(.==100, -105, .)))

#save as file R object
saveRDS(train_clean, file = "train_clean")
saveRDS(validation_clean, file = "validation_clean")


#SCATTERPLOT OF RSSI AND WAP VALUES
WAP_avg <- train_clean %>% 
  select(starts_with("WAP"))
         
df2 <- gather(WAP_avg, key=WAP, value=RSSI) %>% 
  subset(RSSI!=-105)

ggplot(data = df2) +
  geom_point(mapping = aes(x = WAP, y = RSSI))

#CHECKING FOR DUPLICATE OBSERVATIONS

user_dups <- train_clean[c("USERID", "BUILDINGID", "FLOOR", "TIMESTAMP")]
dup2<- user_dups[duplicated(user_dups),]

not_dups<-user_dups[!duplicated(user_dups),]

train_clean$TIMESTAMP<- as_datetime(train_clean$TIMESTAMP)
train_clean$USERID <- as.factor(train_clean$USERID)
train_clean$BUILDINGID <- as.factor(train_clean$BUILDINGID)


dups3 <- train_clean %>% 
        group_by(USERID, TIMESTAMP, BUILDINGID, FLOOR) %>%
        summarise(count = n())


TIME_USER_PLOT<-ggplot(dups3, aes(USERID, TIMESTAMP, color = BUILDINGID, size = count))


TIME_USER_PLOT+
  geom_jitter()+
  scale_y_datetime(date_breaks = "1 day")

TIME_USER_PLOT +
  geom_point(aes(size(count)))

             