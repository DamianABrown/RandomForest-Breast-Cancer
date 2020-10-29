rm(list = ls())
library(tidyverse)
library(randomForest)
library(party)
library(methods)
library(grid)

dataset <- read.csv("C:/Users/damia/Desktop/Random_Projects/Kaggle Datasets/Wisconsin Breast Cancer/data.csv")

#View data to see variables
head(dataset)
summary(dataset)

#Split data into two groups of diagnosis --- B=Benign, M=Malignant
data.m <- subset(dataset, diagnosis == 'M')
data.b <- subset(dataset, diagnosis == 'B')

#Scrambling data before taking test and train data
data.m <- data.m[sample(nrow(data.m)),]
data.b <- data.b[sample(nrow(data.b)),]

#Extracting train (70) and test (30) data
m.train.test <- sample(c(0,1), nrow(data.m), replace = TRUE, prob = c(0.3, 0.7))
sum(m.train.test) #Number of 1's in vector
b.train.test <- sample(c(0,1), nrow(data.b), replace = TRUE, prob = c(0.3, 0.7))
sum(b.train.test) #Number of 1's in vector

#Making new variable of 1's and 0's in each dataset of diagnosis for easy split to test and train datasets
###Inserting 1's and 0's vector into NA column in each dataset
data.m[,33] <- m.train.test
data.b[,33] <- b.train.test
remove(m.train.test, b.train.test)

#Splitting data into Varibles defined as mean, standared error, worst
###The size, shape, texture, etc. of each patients tumor was measured mulitple times and this dataset has each patients
###tumors average, standard error, and worst measurements.
data.m.mean <- subset(data.m, select = c(1:12, 33))
data.m.se <- subset(data.m, select = c(1, 2, 13:22, 33))
data.m.worst <- subset(data.m, select = c(1, 2, 23:33))
data.b.mean <- subset(data.b, select = c(1:12, 33))
data.b.se <- subset(data.b, select = c(1, 2, 13:22, 33))
data.b.worst <- subset(data.b, select = c(1, 2, 23:33))

#Combining diagnosis dataset and splitting data into training data and testing data
#Applying test train vector to main dataset and splitting
dataset <- rbind(data.m, data.b)
dataset[,2] <- as.factor(dataset[,2])
main.train <- subset(dataset, X == 1)
main.test <- subset(dataset, X == 0)
data.mean <- rbind(data.b.mean, data.m.mean)
data.mean[,2] <- as.factor(data.mean[,2])
mean.train <- subset(data.mean, X == 1)
mean.test <- subset(data.mean, X == 0)
data.se <- rbind(data.b.se, data.m.se)
data.se[,2] <- as.factor(data.se[,2])
se.train <- subset(data.se, X == 1)
se.test <- subset(data.se, X == 0)
data.worst <- rbind(data.b.worst, data.m.worst)
data.worst[,2] <- as.factor(data.worst[,2])
worst.train <- subset(data.worst, X == 1)
worst.test <- subset(data.worst, X == 0)

#Running Worst training dataset into Random Forest then using model on test data
ran.forest.worst <- randomForest(diagnosis ~ ., data = worst.train)
print(ran.forest.worst)
print(importance(ran.forest.worst,type = 2))
pred.worst <- predict(ran.forest.worst, worst.test)
table(worst.test[,2], pred.worst)
worst.accuracy = mean(worst.test[,2] == pred.worst)

#Running Standard Error training dataset into Random Forest then using model on test data
ran.forest.se <- randomForest(diagnosis ~ ., data = se.train)
print(ran.forest.se)
print(importance(ran.forest.se,type = 2))
pred.se <- predict(ran.forest.se, se.test)
table(se.test[,2], pred.se)
se.accuracy = mean(se.test[,2] == pred.se)

#Running Mean training dataset into Random Forest then using model on test data
ran.forest.mean <- randomForest(diagnosis ~ ., data = mean.train)
print(ran.forest.mean)
print(importance(ran.forest.mean,type = 2))
pred.mean <- predict(ran.forest.mean, mean.test)
table(mean.test[,2], pred.mean)
mean.accuracy <- mean(mean.test[,2] == pred.mean)

#Printing each model accuracy for comparison
print(paste("MODEL ACCURACY",
            'RF Mean Accuracy:', round(mean.accuracy, 2),
            'RF SE Accuracy:', round(se.accuracy, 2),
            'RF Worst Accuracy:', round(worst.accuracy, 2), collapse = '\n'))


#Running a Random Forest with the main dataset without splitting mean, se, and worst variables
ran.forest.main <- randomForest(diagnosis ~ ., data = main.train)
print(ran.forest.main)
print(importance(ran.forest.main, type = 2))
pred.main <- predict(ran.forest.main, main.test)
table(main.test[,2], pred.main)
main.accuracy <- mean(main.test[,2] == pred.main)

print(cat("Model Accuracy(non seperated):", '\n', "    ", round(main.accuracy, 3), '\n',
          "Model Accuracy (Mean):", '\n', "    ", round(mean.accuracy, 2), '\n',
          "Model Accuracy (Standard Error):", '\n', "    ", round(se.accuracy, 2), '\n',
          "Model Accuracy (Worst):", '\n', "    ", round(worst.accuracy, 3)))     

worst.table <- table(worst.test[,2], pred.worst)
main.table <- table(main.test[,2], pred.main)

print(worst.table)
print(main.table)
