#Module 2
#Predicting Brand Preference

library(readr)
library(ggplot2)
library(dplyr)
library(caret)
install.packages("lattice")
library(lattice)

response <- read_csv("Data/CompleteResponses.csv")
