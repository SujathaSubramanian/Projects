# KAGGLE COMPETITION - GETTING STARTED
library(ROCR)
library(tm)
library(rpart)
library(rpart.plot)
library(randomForest)
library(wordcloud)
library(e1071)
library(caTools)
library(caret)
library(C50)
library(irr)
library(arm)
library(dynamicTreeCut)
library(pROC)

convert_counts <- function(x){
  x<- ifelse(x>0,1,0)
  x<- factor(x,levels=c(0,1),labels=c("No","Yes"))
  return(x)
}

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

##NewsTrain = NewsTrain[order(NewsTrain$PubDate),]
#NewsTest = NewsTest[order(NewsTest$PubDate),]

NewsData = rbind(dplyr::select(NewsTrain,-Popular), NewsTest)

NewsData2 = data.frame(id = NewsData$UniqueID)

CorpusHeadline = Corpus(VectorSource(c(NewsData$Headline)))
CorpusAbstract = Corpus(VectorSource(c(NewsData)$Abstract))

#Pre-Processing for Headline
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
#CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
dtmHeadline = DocumentTermMatrix(CorpusHeadline)
sparseHeadline = removeSparseTerms(dtmHeadline, 0.99)
HeadlineWords = as.data.frame(as.matrix(sparseHeadline))
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))
colnames(HeadlineWords) = paste0("Head_", colnames(HeadlineWords))
#wordcloud(colnames(HeadlineWords),colSums(HeadlineWords),scale=c(4, 0.25),min.freq=100,max.words=10)
NewsData2 = cbind(NewsData2,HeadlineWords)

#Pre-Processing for Abstract
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(CorpusAbstract)
sparseAbstract = removeSparseTerms(dtmAbstract, .99)
AbstractWords = as.data.frame(as.matrix(sparseAbstract))
colnames(AbstractWords) = make.names(colnames(AbstractWords))
colnames(AbstractWords) = paste0("Abs_", colnames(AbstractWords))
NewsData2 = cbind(NewsData2,AbstractWords)

rm(CorpusHeadline,dtmHeadline,sparseHeadline)
rm(CorpusAbstract,dtmAbstract,sparseAbstract)

#Words = data.frame(apply(Words,MARGIN=2,convert_counts))

NewsData2$section = paste0(NewsData$NewsDesk, "#", NewsData$SectionName, "#", NewsData$SubsectionName)


## merge some sections 
NewsData2$section <-
  plyr::revalue(NewsData2$section, c(
    "Business##" = "Business#Business Day#Dealbook",
    "#Business Day#Small Business" = "Business#Business Day#Small Business",
    "#Crosswords/Games#" = "Business#Crosswords/Games#",
    "Culture##" = "Culture#Arts#",
    "Foreign#World#" = "Foreign##",
    "#Health#" = "Science#Health#",
    "National##" = "Other",
    "National#U.S.#Politics" = "Other",
    "OpEd##" = "OpEd#Opinion#",
    "#Open#" = "Other",
    "#Opinion#" = "#Opinion#Room For Debate",
    "#Opinion#The Public Editor" = "OpEd#Opinion#",
    "Science##" = "Science#Health#",
    "Sports##" = "Other",
    "Sports#Sports#" = "Other",
    "Styles#Health#" = "Styles##",
    "Styles#Style#Fashion & Style" = "Styles##",
    "#Travel#" = "Travel#Travel#",
    "#U.S.#" = "#U.S.#Education",
    "#Arts#" = "Culture#Arts#",
    "#Business Day#Dealbook" = "Business#Business Day#Dealbook",
    "#N.Y. / Region#" = "Metro#N.Y. / Region#",
    "#Technology#" = "Business#Technology#",
    "#World#Asia Pacific" = "Foreign#World#Asia Pacific"
  ))

head_names = names(NewsData2)
head_names = head_names[grep( "Head_",  head_names )]
snip_names = names(NewsData2)
snip_names = snip_names[grep( "Abs_",  snip_names )]


NewsData2$clusterid = 1

for (section in c("##", "Business#Business Day#Dealbook", "Business#Business Day#Small Business",
                  "Business#Technology#", "Culture#Arts#", "Metro#N.Y. / Region#",
                  "OpEd#Opinion#")) {
  
  distances = dist(NewsData2[NewsData2$section==section,c(head_names,snip_names)], method = "binary")
  clusters = hclust(distances, method = "ward.D2") 
  
  #clusterGroups = cutree(clusters, k=7)
  clusterGroups = cutreeDynamic(clusters, minClusterSize=20, method="tree", deepSplit=0)
  clusterGroups[clusterGroups==0] = 1
  #summary(factor(clusterGroups))
  
  # add to NewsData2 - then split the data again
  NewsData2[NewsData2$section==section,]$clusterid = clusterGroups
}
unique(NewsData2$clusterid)

rm(clusterGroups,clusters,distances,section)

# split the again
NewsData2$clusterid = factor(NewsData2$clusterid)


NewsData$PubDate = strptime(NewsData$PubDate, "%Y-%m-%d %H:%M:%S")


# Now that R understands this field, there are many different attributes of the date and time that you can extract.
# For example, you can add a variable to your datasets called "Weekday" that contains the day of the week that the article was published (0 = Sunday, 1 = Monday, etc.), by using the following commands:

NewsData2$DWeekday = ifelse(NewsData$PubDate$wday %in% c(1:5),1,0)
NewsData2$Dhour = NewsData$PubDate$hour

NewsData2$WordCount = NewsData$WordCount
NewsData2$logWordCount = log(.001+NewsData$WordCount)

NewsData2$logWordCount = log(.001+NewsData$WordCount)

count.c = function(col, c) {
  sapply(sapply(gregexpr(paste0("[",c,"]"), col), function(x) { as.integer(x>=0) * length(x) } ), max)
}

NewsData2$head.question = count.c(NewsData$Headline, "?")

train = head(NewsData2,nrow(NewsTrain))
test = tail(NewsData2,nrow(NewsTest))

train$Popular = as.factor(NewsTrain$Popular)


#inTrain = createDataPartition(train$Popular,p=.90,list=FALSE)
#WordTrain<- train[inTrain,]
#WordCV <- train[-inTrain,]


#Model 1 - bayesglm using 10-fold Cross Validation
set.seed(123)
k=10
n=floor(nrow(WordTrain)/k) # is the size of each fold

err.vect = rep(NA,k)
err.accuracy = rep(NA,k)
i=1
#now partition each fold
for(i in 1:k)
{
  s1=((i-1)*n+1)
  s2=(i*n)
  subset=s1:s2
  cv.train= WordTrain[-subset,]
  cv.test = WordTrain[subset,]
  fit = bayesglm(Popular~., data=cv.train,na.action=na.omit,family="binomial")
  predRF = predict(fit,newdata=cv.test,type="response")
  predRFROCR = prediction(predRF, cv.test$Popular)
  perfRFROCR = performance(predRFROCR, "tpr", "fpr")
  plot(perfRFROCR)
  err.vect[i]=as.numeric(performance(predRFROCR, "auc")@y.values)
  err.accuracy[i] = confusionMatrix(round(predRF), cv.test$Popular)$overall[1]
  
  print(paste("AUC for fold",i,":",err.vect[i]))
}

print(paste("Average AUC",mean(err.vect)))
print(paste("Average Accuracy",mean(err.accuracy)))

train$section = as.factor(train$section)
test$section = as.factor(test$section)

str(train[,250:261])
kfold(data=train, model=bayesglm, family="binomial", 
      pargs=list(type="response"), formula= Popular ~ section+clusterid + (head.question>0)+logWordCount+DWeekday)


finalModelBayes = bayesglm(Popular~section*clusterid + head.question+section:logWordCount+DWeekday, data=train,family="binomial")
summary(finalModelBayes)
predTestBayes = predict(finalModelBayes,newdata=test, type="response") 


kfold(train,randomForest,pargs=list(type="prob"),nodesize=1,ntree=500,formula=Popular~section+clusterid + head.question+logWordCount+DWeekday)

finalModelRF = randomForest(Popular~section+clusterid + head.question+logWordCount+DWeekday, data=train)
predTestRF = predict(finalModelRF,newdata=test, type="prob")[,2] 





##Caret package using GBM model
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  
  ## repeated ten times
  repeats = 1)


set.seed(825)
gbmFit1 <- train(Popular ~ section*logWordCount+section:clusterid+section:DWeekday+(head.question>0), data = train,
                 method = "gbm",
                 preProcess=c("center","scale"),
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE,
                 metric="ROC")

summary(gbmFit1)

rfFit1 <- train(Popular ~ section*logWordCount+section:clusterid+section:DWeekday+(head.question>0), data = train,
                 method = "rf",
                 preProcess=c("center","scale"),
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE,
                 metric="ROC")


resamps <- resamples(list(GBM = gbmFit3,
                          SVM = svmFit,
                          RDA = rdaFit))
resamps



#Create the final  predictions probabilites and save as a csv file




##Ensemble of RandomForest and GBM

ensemble=(0.6*predTestBayes+0.4*predTestRF)/2

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = ensemble)
write.csv(MySubmission, "Submission-2.csv", row.names=FALSE)


