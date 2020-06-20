require(RColorBrewer)
require(caret)
require(dplyr) # or library("tidyverse")
require(data.table)

#Load data
require(AppliedPredictiveModeling)
data(AlzheimerDisease)

#split into training and test
set.seed(3433)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3 / 4)[[1]]
training = adData[inTrain, ]
testing = adData[-inTrain, ]

#Subset down to relevant predictors Pre-process data

colnames(training)
str(training)
training.sub<-training[,c(1,58:69)]
testing.sub <- testing[,c(1,58:69)] 

##PCA Transform
pp_PCA <-
  preProcess(training.sub[,-1],
             method = c("center", "scale", "pca"),
             thresh = 0.8)
pp_PCA
TrainingTransformed_PCA<-predict(pp_PCA,newdata=training.sub)
TestTransPCA <- predict(pp_PCA,newdata=testing.sub)
  
  
#TrainingTransformed_PCA_1 <- merge(TrainingTransformed_PCA,training.sub[,1],by=0)

##Non PCA Transform

pp_Orig <-
  preProcess(training.sub[,-1],
             method = c("center", "scale"),
             )
pp_Orig
TrainingTransformed_Orig<-predict(pp_Orig,newdata=training.sub)
TestTrasOrig <- predict(pp_Orig,newdata=testing.sub)

#Build Models


modelfit <-
  train(
    diagnosis ~ .,
    data = TrainingTransformed_Orig,
    method = "glm",
  )

modelfit

modelfitPCA <-
  train(
    diagnosis ~ .,
    data = TrainingTransformed_PCA,
    method = "glm",
  )

modelfitPCA


#variable importance
glm_import <- varImp(modelfit, scale = FALSE)
glm_import

glm_import_pca <- varImp(modelfitPCA, scale = FALSE)
glm_import_pca

#performance measures 

az_pred_glm <- predict(modelfit, TestTrasOrig)
az_pred_glmPCA <- predict(modelfitPCA, TestTransPCA)

confusionMatrix(data = az_pred_glm, reference = TestTrasOrig$diagnosis)
confusionMatrix(data = az_pred_glmPCA, reference = TestTransPCA$diagnosis)

