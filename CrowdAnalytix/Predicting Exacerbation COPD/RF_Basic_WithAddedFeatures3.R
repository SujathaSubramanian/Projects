library(caret)
library(pROC)
library(ROCR)

###
###CrowdAnalytix Contest
###Using the observational data from patients with a specific 
###respiratory disease and predicting which patients are more 
###likely to exhibit an exacerbation 
###
###

COPDTrain = read.csv("CAX_COPD_TRAIN_data.csv",na.strings = c("","NA") ,stringsAsFactor= FALSE)
COPDTest = read.csv("CAX_COPD_TEST_data.csv",na.strings = c("","NA"), stringsAsFactor=FALSE)


Train = COPDTrain
Test = COPDTest
prop.table(table(Train$Exacer))

nrow(COPDTest)

library(ggplot2)
qplot(Train$Exacer,Train$SmokHis1,geom="boxplot")
boxplot(SmokHis1+SmokHis2+SmokHis3+SmokHis4~Exacer,data = Train)

class(Train$Exacer)
boxplot(SmokHis3~Exacer,data = Train)
par(mfrow=c(2,2))
hist(Train$SmokHis1,breaks = 5)
hist(Train$SmokHis2,breaks = 5)
hist(Train$SmokHis3,breaks = 5)
hist(Train$SmokHis4,breaks = 5)
dev.off()

ggplot(Train,aes(x=ResQues1a+ResQues1b+ResQues1c+ResQues2a,y=Exacer)) + 
  xlab("Response") + 
  geom_point(position=position_jitter(width = 0.3,height=0.06),
               alpha = 0.4,shape=21,size=1.5) +
  stat_smooth(method=glm,family=binomial,fullrange=TRUE)


##
##75% of the training data is used for model training and remainder is used to evaluate model performance.
##Function creates random splits in each class so that overall class distribution is maintained.
##
inTrain <- createDataPartition(Train$Exacer,p=.75)
training <- Train[inTrain[[1]], ]
testing <- Train[-inTrain[[1]], ]
prop.table(table(training$Exacer))
prop.table(table(testing$Exacer))

##
##Predictor variables in the training set are transformed by centering and scaling using the preProcess function 
##and same is used for predicting the testing set.
##
pp <- preProcess(training[,-c(1:2)])
training2 <- predict(pp, training[,-c(1:2)])
training2$Exacer <- training$Exacer
testing2 <- predict(pp, testing[,-c(1:2)])
testing2$Exacer <- testing$Exacer


##
##Adding new aggregate columns and making them binary
##
LungFunAvg = rowMeans(training2[,c("LungFun9","LungFun14","LungFun4","LungFun3","LungFun12")])
DemoAvg = rowMeans(training2[,c("Demo2","Demo3","Demo4","Demo6")])
RespAvg = rowMeans(training2[,c("ResQues1a","ResQues1b","ResQues1c")])

training2$LowLungFun = ifelse(LungFunAvg<0 ,1,0)
training2$LowDemo = ifelse(DemoAvg<0 ,1,0)
training2$LowRespAvg = ifelse(RespAvg<0 ,1,0)

LungFunAvg = rowMeans(testing2[,c("LungFun9","LungFun14","LungFun4","LungFun3","LungFun12")])
DemoAvg = rowMeans(testing2[,c("Demo2","Demo3","Demo4","Demo6")])
RespAvg = rowMeans(testing2[,c("ResQues1a","ResQues1b","ResQues1c")])
testing2$LowLungFun = ifelse(LungFunAvg<0 ,1,0)
testing2$LowDemo = ifelse(DemoAvg<0 ,1,0)
testing2$LowRespAvg = ifelse(RespAvg<0 ,1,0)

LungFunAvg = rowMeans(Test[,c("LungFun9","LungFun14","LungFun4","LungFun3","LungFun12")])
DemoAvg = rowMeans(Test[,c("Demo2","Demo3","Demo4","Demo6")])
RespAvg = rowMeans(Test[,c("ResQues1a","ResQues1b","ResQues1c")])
Test$LowLungFun = ifelse(LungFunAvg<0 ,1,0)
Test$LowDemo = ifelse(DemoAvg<0 ,1,0)
Test$LowRespAvg = ifelse(RespAvg<0 ,1,0)


nmin <- sum(training2$Exacer == "1")
nmin

##
#Making the predictor as categorical variable
##
training2$Exacer <- factor(training$Exacer, levels=c(0,1), labels=c("No", "Yes"))
testing2$Exacer <- factor(testing$Exacer, levels=c(0,1), labels=c("No", "Yes"))

set.seed(100)

##
##Training using the Random Forest Ensemble. 
##

ctrl <- trainControl(method = "cv",
                     classProbs = TRUE,
                     number=10, 
                     repeats=10, 
                     summaryFunction = twoClassSummary)


rfDownsampled <- train(Exacer ~ ., data = training2,
                       method = "rf",
                       ntree = 1500,
                       tuneLength = 5,
                       metric = "ROC",
                       trControl = ctrl,
                       ## Tell randomForest to sample by strata. Here, 
                       ## that means within each class
                       strata = training2$Exacer,
                       ## Now specify that the number of samples selected
                       ## within each class should be the same
                       sampsize = rep(nmin, 2),
                       importance = TRUE)

rfDownsampled$finalModel
downProbs <- predict(rfDownsampled, testing2, type = "prob")[,2]
table(testing2$Exacer,downProbs > 0.5)
varImp(rfDownsampled)

downsampledROC <- roc(response = testing2$Exacer, 
                      predictor = downProbs,
                      levels = rev(levels(testing2$Exacer)))


predDown <- prediction(downProbs,testing2$Exacer)
perfDown <- performance(predDown, "tpr", "fpr")
plot(perfDown,col = rgb(1, 0, 0, .5), lwd = 2)

getTrainPerf(rfDownsampled)
auc(downsampledROC)


downProbs <- predict(rfDownsampled, newdata=Test, type = "prob")[,2]
SubmitData = data.frame(CAX_ID = COPDTest$CAX_ID)
SubmitData$Exacer = downProbs
##AUC in LeaderBoard = .745
write.csv(SubmitData, file = "submission_16_03.csv", row.names = FALSE) 

##Leaderboard .752

##Private Leaderboard 




