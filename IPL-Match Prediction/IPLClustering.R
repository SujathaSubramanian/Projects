library(xlsx)
library(plyr)
PlayersData = read.xlsx("PlayersData.xlsx",1,stringsAsFactors=F)
save(PlayersData,file="IPLPlayerData.RData")
load("IPLPlayerData.RData")
#IPLPlayerData = subset(PlayersData,format=="TWENTY20")
IPLPlayerData = PlayersData
IPLPlayerData[is.na(IPLPlayerData)] = 0

IPLPlayerData$Year = as.factor(IPLPlayerData$Year)
IPLPlayerData$format = as.factor(IPLPlayerData$format)
IPLPlayerData$Runs = as.numeric(IPLPlayerData$Runs)
IPLPlayerData$Balls = as.numeric(IPLPlayerData$Balls)
IPLPlayerData$Outs = as.numeric(IPLPlayerData$Outs)
IPLPlayerData$Strike.Rate = as.numeric(IPLPlayerData$Strike.Rate)
IPLPlayerData$Batting.Average = as.numeric(IPLPlayerData$Batting.Average)
IPLPlayerData$Overs = as.numeric(IPLPlayerData$Overs)
IPLPlayerData$BowlRuns = as.numeric(IPLPlayerData$BowlRuns)
IPLPlayerData$Wickets = as.numeric(IPLPlayerData$Wickets)
IPLPlayerData$Economy = as.numeric(IPLPlayerData$Economy)
IPLPlayerData$Bowling.Average = as.numeric(IPLPlayerData$Bowling.Average)
IPLPlayerData$Bowl.SR = as.numeric(IPLPlayerData$Bowl.SR)


meltData <- melt(IPLPlayerData, id.vars=c(1:2,14))
unique(meltData$variable)
castData = dcast(meltData, PlayerName ~ format+Year+variable, fun.aggregate = sum)
str(castData)
castData = castData[,-c(2:12)]

##Use Hierarchical Clustering
# Compute distances

library(caret)


preproc = preProcess(castData[,-1])

castDataNorm = predict(preproc, castData[,-1])
summary(castDataNorm)

distances = dist(castDataNorm, method = "euclidean")

# Hierarchical clustering
clusterPlayers = hclust(distances, method = "ward.D") 

# Plot the dendrogram
plot(clusterPlayers)

# Assign points to clusters
clusterGroups = cutree(clusterPlayers, k = 6)

clusterGroups[[1]]

#Now let's figure out what the clusters are like.
# Let's use the tapply function to compute the percentage of movies in each genre and cluster


table(clusterGroups)


#5 players ingroup1

x= subset(castData,clusterGroups==1)
x$PlayerName

x= subset(castData,clusterGroups==6)
x$PlayerName



