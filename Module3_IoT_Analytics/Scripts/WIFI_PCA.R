#Title:       PCA:Feasibility of 'Wifi Fingerprinting' to Determine Person's Indoor Location
#Version:     1.0
#Date:        April 15, 2020

library(caret)
library(tidyverse)
library(dplyr)
library(caret)

#read saved R object
train_set <- readRDS("train_clean")
validate_set <- readRDS("validation_clean")

#PRINCIPAL COMPONENT ANALYSIS

pca_scale <- prcomp(train_set[11:ncol(train_set)], scale = T)
pca <- prcomp(train_set[11:ncol(train_set)], scale = F)

#VARIANCE OF PCA
pca_var <-pca$sdev^2
pca_var_per <- round((pca_var/sum(pca_var))*100, 2)
sum(pca_var_per[1:120])

#VISUAL EXPLORATION OF PCA
barplot(pca_var_per, 
        main = "Plots", 
        xlab = "PCA", 
        ylab = " Percentage of Variation")

#KEEPING ONLY FIRST 120 COLS IN TRAIN SET
n = 120
pca_train <- as_tibble(pca$x) %>% 
              select(PC1:all_of(n))

#APPLY PCA TO VALIDATION SET
validation_WAPS <- validate_set %>% 
                    select(11:ncol(validate_set))

pca_validation <- as_tibble(predict(pca, newdata = validation_WAPS)) %>% 
                  select(PC1:all_of(n))

saveRDS(pca_train, file = "pca_train")
saveRDS(pca_validation, file = "pca_validation")

