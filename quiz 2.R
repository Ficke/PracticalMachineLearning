require(Hmisc)
require(RColorBrewer)
library(caret)
library("dplyr") # or library("tidyverse")

#Load data 
require(AppliedPredictiveModeling)
data(AlzheimerDisease)

#split into training and test 
set.seed(3433)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]



#invstigate 1 way relationships using quantile data
training <- training %>% mutate(id = row_number())
names(training)
grouped<-as.data.frame(sapply(training[,-c(9,10)],ntile,n=5))
grouped<-as.data.frame(sapply(grouped,as.factor))
grouped <- grouped %>% mutate(id = row_number())
data<-merge(grouped,training,by ="id")
names(data)

ggplot(data = data, aes(x = id, y = CompressiveStrength, color = Age.x)) + 
  geom_point() + 
  scale_color_brewer(palette="Dark2")

ggplot(data = data, aes(x = id, y = CompressiveStrength, color = FlyAsh.x)) + 
  geom_point() + 
  scale_color_brewer(palette="Dark2")

ggplot(data = data, aes(x = id, y = CompressiveStrength)) + 
  geom_point() + 
  scale_color_brewer(palette="Dark2")


#question 3

names(training)
histogram(training$Superplasticizer)

#4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

names(training)
training.sub<-training[,c(1,58:69)]
preProcPCA <- preProcess(training.sub, method = c("center", "scale","pca"),thresh=0.8)
trainTransformed <- predict(preProcValues, training.sub)
?preProcess

summary(trainTransformed)

#question 5
training.sub<-training[,c(1,58:69)]
test.sub<-testing[,c(1,58:69)]

#dummyvariables 


#preprocess for PCA & not
preProc<- preProcess(training.sub, method = c("center", "scale"))
preProcPCA <- preProcess(training.sub, method = c("center", "scale","pca"),thresh=0.8)


trainTransformed <- predict(preProc, training.sub)
testTransformed <- predict(preProc, test.sub)
 
trainTransformedPCA <- predict(preProcPCA, training.sub)
testTransformedPCA <- predict(preProcPCA, test.sub)

#modeltrain

glm <- train(diagnosis ~ ., data = trainTransformed, method = "glm")
glmPCA <- train(diagnosis ~ ., data = trainTransformedPCA, method = "glm")

glmPredict<-predict(glm,testTransformed)
glmPCAPredict<-predict(glmPCA,testTransformedPCA)

confusionMatrix(data=)