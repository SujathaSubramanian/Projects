# KAGGLE COMPETITION - GETTING STARTED
library(ROCR)
library(tm)
library(rpart)
library(rpart.plot)
library(randomForest)

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

# Weekday could now be used as an independent variable in your predictive models.
# For more fields that you can extract from a date/time object in R, see the help page ?POSIXlt



str(NewsTrain)
CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
CorpusSnippet = Corpus(VectorSource(c(NewsTrain$Snippet, NewsTest$Snippet)))
CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))

CorpusHeadline[[5]]
CorpusSnippet[[5]]
CorpusAbstract[[5]]

#Pre-Processing for Headline
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
dtmHeadline = DocumentTermMatrix(CorpusHeadline)
sparseHeadline = removeSparseTerms(dtmHeadline, 0.99)
HeadlineWords = as.data.frame(as.matrix(sparseHeadline))
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))
colnames(HeadlineWords) = paste0("H", colnames(HeadlineWords))

#Pre-Processing for Snippet
CorpusSnippet = tm_map(CorpusSnippet, tolower)
CorpusSnippet = tm_map(CorpusSnippet, PlainTextDocument)
CorpusSnippet = tm_map(CorpusSnippet, removePunctuation)
CorpusSnippet = tm_map(CorpusSnippet, removeWords, stopwords("english"))
CorpusSnippet = tm_map(CorpusSnippet, stemDocument)
dtmSnippet = DocumentTermMatrix(CorpusSnippet)
sparseSnippet = removeSparseTerms(dtmSnippet, 0.96)
SnippetWords = as.data.frame(as.matrix(sparseSnippet))
colnames(SnippetWords) = make.names(colnames(SnippetWords))
colnames(SnippetWords) = paste0("S", colnames(SnippetWords))

#Pre-Processing for Abstract
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(CorpusAbstract)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.96)
AbstractWords = as.data.frame(as.matrix(sparseAbstract))
colnames(AbstractWords) = make.names(colnames(AbstractWords))
colnames(AbstractWords) = paste0("A", colnames(AbstractWords))

Words = cbind(HeadlineWords,SnippetWords,AbstractWords,row.names=NULL)


# Now we need to split the observations back into the training set and testing set.
WordTrain = head(Words, nrow(NewsTrain))
WordTest = tail(Words, nrow(NewsTest))

NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

# Now that R understands this field, there are many different attributes of the date and time that you can extract.
# For example, you can add a variable to your datasets called "Weekday" that contains the day of the week that the article was published (0 = Sunday, 1 = Monday, etc.), by using the following commands:

WordTrain$DWeekday = NewsTrain$PubDate$wday
WordTest$DWeekday = NewsTest$PubDate$wday

WordTrain$Dmday = NewsTrain$PubDate$mday
WordTest$Dmday = NewsTest$PubDate$mday

WordTrain$Dmonth = NewsTrain$PubDate$month
WordTest$Dmonth = NewsTest$PubDate$month

WordTrain$Dyday = NewsTrain$PubDate$yday
WordTest$Dyday = NewsTest$PubDate$yday

WordTrain$Dyear = NewsTrain$PubDate$year
WordTest$Dyear = NewsTest$PubDate$year

WordTrain$Dhour = NewsTrain$PubDate$hour
WordTest$Dhour = NewsTest$PubDate$hour

# Before building models, we want to add back the original variables from our datasets. We'll add back the dependent variable to the training set, and the WordCount variable to both datasets. You might want to add back more variables to use in your model - we'll leave this up to you!
WordTrain$Popular = as.factor(NewsTrain$Popular)
WordTrain$WordCount = NewsTrain$WordCount
WordTest$WordCount = NewsTest$WordCount

str(WordTrain)
#Model 1 - glm
Log = glm(Popular ~ ., data=WordTrain, family=binomial)
summary(Log)
predLog = predict(Log, type="response")
table(NewsTrain$Popular,predLog > 0.5)
(5340+31)/nrow(NewsTrain)
(5321+95)/nrow(NewsTrain)
(5313+138)/nrow(NewsTrain)

#Model 2 - CART
str(WordTrain)
CART = rpart(Popular~., data=WordTrain, method="class")

summary(CART)
prp(CART)
predCART = predict(CART,type="prob")[,2]
table(WordTrain$Popular,predCART > 0.5)


predCARTROCR = prediction(predCART, WordTrain$Popular)
perfCARTROCR = performance(predCART, "tpr", "fpr")
plot(perfCARTROCR, colorize=TRUE)
as.numeric(performance(predCARTROCR, "auc")@y.values)


#Model 3 - RandomForest
RF = randomForest(Popular~., data=WordTrain) 
RF
predRF = predict(RF, type="prob")[,2] 

table(WordTrain$Popular,predRF > 0.5)
(5282+245)/nrow(WordTrain)



plot(perfROCR, colorize=TRUE)

predRFROCR = prediction(predRF, WordTrain$Popular)
perfRFROCR = performance(predRFROCR, "tpr", "fpr")
plot(perfRFROCR, colorize=TRUE)

as.numeric(performance(predRFROCR, "auc")@y.values)



#Creating predictions probabilites and save as a csv file
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTestWords)
write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)




