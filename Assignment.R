setwd("~/Documents/Coursera/Data Science/Machine Learning/Week 4/Assignment")

set.seed(8675)
ggplot2
library(caret)
library(doParallel)

#setup parallel processing
cluster<- makeCluster(detectCores())
registerDoParallel(cluster)

pmlData <- read.csv("pml-training.csv",stringsAsFactors=TRUE, na.strings=c("NA"))

#### FEATURE SELECTION ####
#Remove first 5 columns (no predictability from these: name, timestamp, etc.)
pmlData <- pmlData[,-(1:5)]

#Remove columns with low variability
pmlData <- pmlData[,-nearZeroVar(pmlData)]

#Remove columns with NA content 
pmlData <- pmlData[,colSums(is.na(pmlData)) == 0]

#### CREATE TRAIN AND TEST SETS ####
inTrain <- createDataPartition(pmlData$classe, p=0.7, list=FALSE)
trainData <- pmlData[inTrain,]
testData <- pmlData[-inTrain,]

#### TRAIN THE MODEL ####
# using Random Forest model using 5 fold cross-validation 
trainControl <-trainControl(method = "cv", number = 5, allowParallel = TRUE)
rfModel <- train(classe ~. , data=trainData, method="rf" , preProcess=c("center","scale"), trControl=trainControl )

#### PREDICT RESERVED TEST SET USING TRAINED MODEL ####
rfTesting <- predict(rfModel, testData)
confusionMatrix(rfTesting, testData$classe)

#### PREDICT HELDOUT DATA SET (Quiz Data) ####
holdout<-read.csv("pml-testing.csv", stringsAsFactors=TRUE )
holdout$classe <- predict(rfModel, holdout)
matrix(holdout$classe)

