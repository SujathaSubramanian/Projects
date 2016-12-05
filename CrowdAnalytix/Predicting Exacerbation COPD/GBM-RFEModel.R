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

training2$Exacer <- factor(training$Exacer, levels=c(0,1), labels=c("No", "Yes"))
testing2$Exacer <- factor(testing$Exacer, levels=c(0,1), labels=c("No", "Yes"))

#training2$Exacer = factor(training2$Exacer)
#testing2$Exacer = factor(testing2$Exacer)

set.seed(100)
rfFuncs$summary <- twoClassSummary

subsets <- c(1:5, 10, 15, 20, 25)

rfe(training2[,-61],training2[,61],rfeControl = rfeControl(rfFuncs), s=subsets,metric="ROC")



fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  classProbs = TRUE,
  repeats = 10,
  summaryFunction = twoClassSummary)

str(training2$Exacer)
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1,
                        n.minobsinnode = 10)

GBMDownsampled <- train(Exacer~ResQues1a+Demo2+Demo4+Dis2Times+Demo6+Demo2*Demo4,
                        data=training2,
                        distribution="bernoulli",
                        method="gbm",
                        preProc = c("center", "scale"),
                        trControl = fitControl,
                        verbose=FALSE,
                        tuneGrid = gbmGrid,
                        metric = "ROC")


summary(GBMDownsampled)
GBMDownsampled

GBMProbs <- predict(GBMDownsampled, newdata=testing2,type="prob")[,2]

table(GBMProbs > 0.5)
table(testing2$Exacer,GBMProbs > 0.5)

(54+5)/496


GBMDownsampledROC <- roc(response = testing2$Exacer, 
                        predictor = GBMProbs)


predDown <- prediction(GBMProbs,testing2$Exacer)
perfDown <- performance(predDown, "tpr", "fpr")
plot(perfDown,col = rgb(1, 0, 0, .5), lwd = 2)

getTrainPerf(GBMDownsampled)
auc(GBMDownsampledROC)



GBMProbs <- predict(GBMDownsampled,newdata=Test, type = "prob")[,2]


SubmitData = data.frame(CAX_ID = COPDTest$CAX_ID)
SubmitData$Exacer = GBMProbs
write.csv(SubmitData, file = "submission_16_01.csv", row.names = FALSE) 
