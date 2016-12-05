#############Modelling and Prediction #################################################



library(xlsx)


PointsTableData = read.xlsx("PointsTableData.xls",1)

test2015 = subset(PointsTableData,Year == 2015 )
names(test2015)[1] = "Match"

PointsTableData = subset(PointsTableData,Year < 2015 )
PointsTableData = subset(PointsTableData,TeamName != "Pune Warriors")
PointsTableData <- PointsTableData[order(PointsTableData$Year),] 


names(PointsTableData)[1] = "Match"
PointsTableData$Match = seq(1:nrow(PointsTableData))
PointsTableData

#MatchResultData$Team1Result = as.factor(MatchResultData$Team1Result)
#MatchResultData = MatchResultData[,-1]

PointsTableData$TS1 = PointsTableData$Total.TWENTY20.Runs*PointsTableData$BSR.TWENTY20
PointsTableData$TS2 = PointsTableData$Total.TWENTY20.Wickets/PointsTableData$Total.TWENTY20.Overs

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(PointsTableData$Points, SplitRatio = 0.7)

# Split up the data using subset
train = subset(PointsTableData, split== TRUE)
test = subset(PointsTableData, split==FALSE)
test2014 = subset(PointsTableData, Year == 2014)

train = subset(PointsTableData, Year %in% c(2009,2010,2011,2012,2013))
test = subset(PointsTableData, Year== 2014)


names(train)

MatchResultDataLog = lm(Points ~ .-(Match+Year+TeamName+Points) ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ TS2 ,data = train)
summary(MatchResultDataLog)


MatchResultDataLog = lm(Points ~ Total.ODI.Runs ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ Total.ODI.Wickets ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ Total.ODI.Overs ,data = train)
summary(MatchResultDataLog)


MatchResultDataLog = lm(Points ~ BA.ODI ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ BA.Twenty20 ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ BowlA.ODI ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ BowlA.TWENTY20 ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ BSR.ODI ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ BSR.TWENTY20 ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ BowlSR.ODI ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ BowlSR.TWENTY20 ,data = train)
summary(MatchResultDataLog)


MatchResultDataLog = lm(Points ~ Economy.ODI ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ Economy.TWENTY20 ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ Total.ODI.Outs ,data = train)
summary(MatchResultDataLog)





##Models with "."
MatchResultDataLog = lm(Points ~ Total.TWENTY20.Runs   ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ Total.TWENTY20.Wickets,data = train)
summary(MatchResultDataLog)


MatchResultDataLog = lm(Points ~ Total.TWENTY20.Overs ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ Total.TWENTY20.Outs ,data = train)
summary(MatchResultDataLog)




MatchResultDataLog = lm(Points ~ TS1 + Total.TWENTY20.Outs,data = train)
summary(MatchResultDataLog)


MatchResultDataLog = lm(Points ~ TS2 ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ TS1+TS2 ,data = train)
summary(MatchResultDataLog)



###final model
MatchResultDataLog = lm(Points ~ Total.TWENTY20.Runs +BowlSR.TWENTY20 ,data = train)
summary(MatchResultDataLog)

MatchResultDataLog = lm(Points ~ Total.TWENTY20.Runs +BowlSR.TWENTY20 ,data = train)
summary(MatchResultDataLog)

predictTest = predict(MatchResultDataLog,newdata=test)
predictTest = predict(MatchResultDataLog,newdata=train)

predictTest = predict(MatchResultDataLog,newdata=test2015)

x = data.frame("TeamName" = train$TeamName,"Prediction"=predictTest,"Actual" = train$Points,"Diff" = predictTest-train$Points)

x = data.frame("TeamName" = test$TeamName,"Prediction"=predictTest,"Actual" = test$Points,"Diff" = predictTest-test$Points)

x = data.frame("TeamName" = test2014$TeamName,"Prediction"=predictTest,"Actual" = test2014$Points,"Diff" = predictTest-test2014$Points)

x = data.frame("TeamName" = test2015$TeamName,"Prediction"=predictTest,"Actual" = test2015$Points,"Diff" = predictTest-test2015$Points)

x[order(x$Actual),] 
x[order(x$Prediction),] 
x[order(x$Diff),] 
