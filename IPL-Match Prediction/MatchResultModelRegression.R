library(xlsx)


MatchResultData = read.xlsx("MatchResultsModelData1.xls",1)

names(MatchResultData)[1] = "Match"
MatchResultData$Match = seq(1:nrow(MatchResultData))

tail(MatchResultData,5)
str(MatchResultData)
MatchResultData$Team1Result = as.factor(MatchResultData$Team1Result)

table(MatchResultData$Team1Result)
#MatchResultData = MatchResultData[,-1]

#ggplot(MatchResultData,aes(x=Match,y=DiffRuns,colour=Team1Result))+geom_point()+facet_grid(.~Team1HomeAway)
#ggplot(MatchResultData,aes(x=Match,y=DiffRuns,colour=Team1Result))+geom_point()
#ggplot(MatchResultData,aes(x=Match,y=TS3,colour=Team1Result))+geom_point()
#ggplot(MatchResultData,aes(x=OSBat,y=Team1Result,color=Team1Result))+geom_point()

#+ geom_point(position=position_jitter(width=0.3,height=0.06),alpha=0.4,shape=21,size=1.5)
#+  stat_smooth(method=glm,family=binomial)
#str(MatchResultData)
#panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
#{
#  usr <- par("usr"); on.exit(par(usr))
#  par(usr = c(0, 1, 0, 1))
#  r <- abs(cor(x, y))
#  txt <- format(c(r, 0.123456789), digits=digits)[1]
#  txt <- paste(prefix, txt, sep="")
#  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
#  text(0.5, 0.5, txt, cex = cex.cor * r)
#}
#pairs(MatchResultData[,c(5:21)],pch=".",upper.panel=panel.cor)
#pairs(MatchResultData[,c("TS3","TS1")],pch=".",upper.panel=panel.cor)
#pairs(~OSBowlers+OSBat,data=MatchResultData,pch=".",upper.panel=panel.cor)

MatchResultData = MatchResultData[,-1]
table(MatchResultData$TS3>0,MatchResultData$Team1Result)



# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
MatchResultData$Team1HomeAway = as.factor(MatchResultData$Team1HomeAway)
split = sample.split(MatchResultData$Team1Result, SplitRatio = 0.7)

# Split up the data using subset
train = subset(MatchResultData, split==TRUE)
test = subset(MatchResultData, split==FALSE)

# Logistic Regression Model
#Best so fat
MatchResultDataLog = glm(Team1Result ~ BattingSR*BowlingSR,data = train, family=binomial)
summary(MatchResultDataLog)


predictTest = predict(MatchResultDataLog, type="response",newdata=test)
table(predictTest>0.57)

# Confusion matrix with threshold of 0.5

table(test$Team1Result, predictTest > 0.5)

table(predictTest<.2,(predictTest >=.2 & predictTest <.4),(predictTest >=.4 & predictTest<.6),(predictTest>=.6 & predictTest<.8),predictTest >=.8)

table(predictTest<.2)
table(predictTest >=.2 & predictTest <.4)
table(predictTest >=.4 & predictTest <.6)
table(predictTest >=.6 & predictTest <.8)
table(predictTest>=.8)

95/(95+63)
      
73/(73+45)
(13+51)/(13+51+19+35)
66/(66+35+17)


55/(55+63)
67/(67+51)
69/(67+51)
73/(67+51)
str(test)

