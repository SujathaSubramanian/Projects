library(caret)
library(pROC)
library(ROCR)
###
###Step 1: Run GBM using all the covariates.
###Step 2: Use 2 types of GBM models. One is balanced by down sampling for mismatch data 
## and second is without  down sampling
###

COPDTrain = read.csv("CAX_COPD_TRAIN_data.csv",na.strings = c("","NA") ,stringsAsFactor= FALSE)
COPDTest = read.csv("CAX_COPD_TEST_data.csv",na.strings = c("","NA"), stringsAsFactor=FALSE)


Train = COPDTrain
Test = COPDTest

inTrain <- createDataPartition(Train$Exacer,p=.75)
training <- Train[inTrain[[1]], ]
testing <- Train[-inTrain[[1]], ]

pp <- preProcess(training[,-c(1:2)])
training2 <- predict(pp, training[,-c(1:2)])
training2$Exacer <- training$Exacer

testing2 <- predict(pp, testing[,-c(1:2)])
testing2$Exacer <- testing$Exacer


nmin <- sum(training2$Exacer == "1")
nmin

set.seed(100)


training2$Exacer <- factor(training$Exacer, levels=c(0,1), labels=c("No", "Yes"))
testing2$Exacer <- factor(testing$Exacer, levels=c(0,1), labels=c("No", "Yes"))


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  classProbs = TRUE,
  repeats = 10,
  summaryFunction = twoClassSummary)



GLMDownsampled <- train(Exacer~ ResQues1a+Demo2+Demo4+Dis2Times+Demo6+Demo2*Demo4+Demo2*Demo6+Demo4*Demo6,
                        data=training2,
                        family="binomial",
                        method="glm",
                        preProc = c("center", "scale"),
                        trControl = fitControl,
                        verbose=FALSE,
                        metric = "ROC")

fit = glm(Exacer~ResQues1a+LungFun9+Demo2+Demo4+Dis2Times+Demo6+Demo2*Demo4,data=training2,family="binomial")#***

summary(fit)


GLMProbs <- predict(fit, newdata=testing2,type="response")

table(GBMProbs > 0.5)
table(testing2$Exacer,GBMProbs > 0.5)

(52+4)/496


GLMDownsampledROC <- roc(response = testing2$Exacer, 
                         predictor = GLMProbs)


predDown <- prediction(GLMProbs,testing2$Exacer)
perfDown <- performance(predDown, "tpr", "fpr")
plot(perfDown,col = rgb(1, 0, 0, .5), lwd = 2)

getTrainPerf(fit)
auc(GLMDownsampledROC)



GLMProbs <- predict(GBMDownsampled,newdata=Test, type = "prob")[,2]


SubmitData = data.frame(CAX_ID = COPDTest$CAX_ID)
SubmitData$Exacer = GBMProbs
write.csv(SubmitData, file = "submission_16_01.csv", row.names = FALSE) 
