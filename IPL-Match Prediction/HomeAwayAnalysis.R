library(ggplot2)
library(plyr)
library(lattice)
library(xlsx)

#################
#Helper Functions
#################
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

#End############


IPLMatchResults = read.xlsx("IPLMatchResults.xlsx",1)
IPLTeamHome = read.xlsx("IPLMatchResults.xlsx",2)
IPLTeamName = read.xlsx("IPLMatchResults.xlsx",3)


IPLMatchResults$Team.1[IPLMatchResults$Team.1 == "Chargers"] = "Sunrisers"
IPLMatchResults$Team.2[IPLMatchResults$Team.2 == "Chargers"] = "Sunrisers"
IPLMatchResults$Winner[IPLMatchResults$Winner == "Chargers"] = "Sunrisers"

unique(IPLMatchResults$Team.1)
unique(IPLMatchResults$Team.2)
unique(IPLMatchResults$Winner)

IPLMatchResults = IPLMatchResults[!IPLMatchResults$Winner %in% c("abandoned","no result","tied" ),]

MatchResults = merge(IPLMatchResults,IPLTeamHome,by.x="Ground",by.y="GroundName")


MatchResults["Loser"] = ifelse(as.character(MatchResults$Winner) == as.character(MatchResults$Team.1), 
                               as.character(MatchResults$Team.2), as.character(MatchResults$Team.1))


MatchResults["HomeAway"] = ifelse(as.character(MatchResults$Winner) == as.character(MatchResults$GroundCode) 
                                  ,"Home",ifelse(as.character(MatchResults$Loser) == as.character(MatchResults$GroundCode),"Away","Neutral"))



#Overall Match - Home Vs Away Comparison
HomeWins = nrow(MatchResults[MatchResults$HomeAway == "Home",])/nrow(MatchResults[!MatchResults$HomeAway == "Neutral",])
AwayWins = nrow(MatchResults[MatchResults$HomeAway == "Away",])/nrow(MatchResults[!MatchResults$HomeAway == "Neutral",])


barplot(c(HomeWins,AwayWins),
        names.arg = c("Home","Away"),
        col=c("green", "red"),#TeamData$HomeAway,
        xlab="Wins",
        ylab="# of Wins",
        main = "Home Vs Away Win Comparison")





#Teamwise - Home Vs Away Comparison                                                                       


WinsDF = data.frame(Team="",
                    Type="",
                    Wins="",
                    stringsAsFactors=FALSE) 

HomeAwayDF = data.frame(Team="",
                        HomeAwayDiff="", 
                        stringsAsFactors=FALSE) 

Teams = unique(MatchResults$Winner)
par(mfrow=c(3,3))
i=1

while( i < length(Teams))
{
  #Total Matches played by a team (excluding the matches played in the "Neutral" ground
  Total = nrow(subset(MatchResults,(Team.1==as.character(Teams[i]) | Team.2== as.character(Teams[i])) & HomeAway != "Neutral"))
  
  #Total Wins by any team at their Home Ground
  #HomeWins = nrow(subset(MatchResults,Winner == Teams[i] & HomeAway == "Home"))/Total
  #AwayWins = nrow(subset(MatchResults,Winner == Teams[i] & HomeAway == "Away"))/Total
  
  HomeWins = nrow(subset(MatchResults,(Team.1==as.character(Teams[i]) | Team.2== as.character(Teams[i])) & HomeAway == "Home"))/Total
  AwayWins = nrow(subset(MatchResults,(Team.1==as.character(Teams[i]) | Team.2== as.character(Teams[i])) & HomeAway == "Away"))/Total
  
  df = c(as.character(Teams[i]), HomeWins-AwayWins)
  HomeAwayDF = rbind(HomeAwayDF,df)
  
  df= c(as.character(Teams[i]), "Home",HomeWins)
  WinsDF = rbind(WinsDF,df)
  df= c(as.character(Teams[i]), "Away",AwayWins)
  WinsDF = rbind(WinsDF,df)
  
  
  barplot(c(HomeWins,AwayWins),
          names.arg = c("Home","Away"),
          col=c("green", "red"),#TeamData$HomeAway,
          xlab="Wins",
          ylab="# of Wins",
          main = paste0("Home Vs Away Win Comparison for ",Teams[i]))
  
  i=i+1
}

HomeAwayDF = HomeAwayDF[-1,]
WinsDF=WinsDF[-1,]


#Home Advantage Statistic
mean(as.numeric(HomeAwayDF$HomeAwayDiff))


g=ggplot(WinsDF,aes(x=Type,y=as.numeric(Wins),fill=Type))
g=g+geom_bar(stat="identity", color="#000000") 
g=g+facet_wrap(~Team, scales="free_x")
g=g+theme_bw() 
#g=g+scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", 
#                                "#33a02c", "#fb9a99", "#e31a1c", 

#g= g + coord_cartesian(ylim = c(0, 2))
g=g+scale_y_continuous(limits = c(0.0,1.0))
g=g+labs(title = "Home Vs Away Comparison")
g=g+xlab("Home Vs Away")
g=g+ylab("Percent Win")
g
