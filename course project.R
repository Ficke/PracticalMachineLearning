#Adam Ficke
# Data sourced from http://groupware.les.inf.puc-rio.br/har

require(caret)
require(dplyr)
require(gbm)
require(ggplot2)
require(hablar)
library(parallel)


#read in data
data <-
  read.csv(
    "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
    stringsAsFactors = FALSE
  )
test <-
  read.csv(
    "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
    stringsAsFactors = FALSE
  )

#clean data for near-zero variance & all NAs
nzvList <- nearZeroVar(data, saveMetrics = TRUE)
nzv <- nearZeroVar(data)

FilteredData <- data[, -nzv]
FilteredTest <- test[, -nzv]

zeros <- colMeans(!is.na(FilteredData)) > .97
summary(zeros)

FilteredData <- FilteredData[zeros]
FilteredTest <- FilteredTest[zeros]

#filter out time information
FilteredData <- FilteredData[, -c(1, 3, 4, 5, 6)]
FilteredTest <- FilteredTest[-c(1, 3, 4, 5, 6)]

str(FilteredData)
str(FilteredTest)

#standardize data types
FilteredData[, 2:53] <- 
  lapply(FilteredData[, 2:53], as.numeric)
FilteredData[, c(1, 54)] <-
  lapply(FilteredData[, c(1, 54)], as.factor)

FilteredTest[, 2:53] <- 
  lapply(FilteredTest[, 2:53], as.numeric)
FilteredTest[, c(1, 54)] <-
  lapply(FilteredTest[, c(1, 54)], as.factor)


#split into training and test
set.seed(250)
trainIndex <- createDataPartition(FilteredData$classe, p = .7, 
                                  list = FALSE, 
                                  times = 1)

train <- FilteredData[trainIndex, ]
valid <- FilteredData[-trainIndex, ]

#EDA 
#one way looks
p <- ggplot(FilteredData, aes(x = kurtosis_roll_belt)) +
  geom_bar()
p

#find clusters of variables 

#select best representatives of each cluster 


#try cross-validated GBM  
library(doParallel)
cl <- makePSOCKcluster(8)
registerDoParallel(cl)


fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)


set.seed(250)
gbmFit1 <- train(
  classe ~ .,
  data = train,
  method = "gbm",
  trControl = fitControl,
  verbose = FALSE,
  na.action = na.pass
)
stopCluster(cl)

summary(gbmFit1)
varImp(gbmFit1)

#Try RF model 

registerDoParallel(cl)

set.seed(250)
rfFit1 <- train(
  classe ~ .,
  data = train,
  method = "rf",
  trControl = fitControl,
  na.action = na.pass
)

stopCluster(cl)
registerDoSEQ()


#Performance
predictions <- predict(gbmFit1,valid,na.action = na.pass)
confusionMatrix(data=predictions,reference = valid$classe)
postResample(pred = predictions, obs = valid$classe)

predictions.rf <- predict(rfFit1,valid,na.action = na.pass)
confusionMatrix(data=predictions.rf,reference = valid$classe)

varImp(gbmFit1)
varImp(rfFit1)

VP <- ggplot(FilteredData, aes(x = kurtosis_roll_belt)) +
  geom_bar()
p

#predict on 20 data points 
predictions.test <- predict(rfFit1,test,na.action = na.pass)
predictions.test

predictions.test2 <- predict(gbmFit1,test,na.action = na.pass)
predictions.test2

#match data types 
