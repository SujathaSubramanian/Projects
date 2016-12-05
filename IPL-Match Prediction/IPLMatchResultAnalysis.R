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


unique(IPLMatchResults$Year)
IPLMatchResults$Team.1[IPLMatchResults$Team.1 == "Chargers"] = "Sunrisers"
IPLMatchResults$Team.2[IPLMatchResults$Team.2 == "Chargers"] = "Sunrisers"
IPLMatchResults$Winner[IPLMatchResults$Winner == "Chargers"] = "Sunrisers"
  
unique(IPLMatchResults$Team.1)
unique(IPLMatchResults$Team.2)
unique(IPLMatchResults$Winner)

IPLMatchResults = IPLMatchResults[!IPLMatchResults$Winner %in% c("abandoned","no result","tied" ),]

MatchResults = merge(IPLMatchResults,IPLTeamHome,by.x="Ground",by.y="GroundName",all.y=FALSE)


MatchResults["LosingTeam"] = ifelse(as.character(MatchResults$Winner) == as.character(MatchResults$Team.1), 
                               as.character(MatchResults$Team.2), as.character(MatchResults$Team.1))


MatchResults["HomeAwayWinner"] = ifelse(as.character(MatchResults$Winner) == as.character(MatchResults$GroundCode) 
                                  ,"Home",ifelse(as.character(MatchResults$LosingTeam) == as.character(MatchResults$GroundCode),"Away","Neutral"))



MatchResults = merge(MatchResults,IPLTeamName,by.x="Team.1",by.y="TeamCode")
MatchResults = rename(MatchResults, c("TeamName"="Team1Name"))
MatchResults = merge(MatchResults,IPLTeamName,by.x="Team.2",by.y="TeamCode")
MatchResults = rename(MatchResults, c("TeamName"="Team2Name"))
MatchResults$Team1Name = as.character(MatchResults$Team1Name)
MatchResults$Team2Name = as.character(MatchResults$Team2Name)

MatchResults$Team1Name[MatchResults$Team1Name == "Sunrisers Hyderabad "] = "Sunrisers Hyderabad"
MatchResults$Team2Name[MatchResults$Team2Name == "Sunrisers Hyderabad "] = "Sunrisers Hyderabad"


#Part B

PastTeamData = read.xlsx("PastTeamData.xls", sheetName = "IPL Team Data",stringsAsFactors = F)
PastTeamData$TeamName[PastTeamData$TeamName == "Sunrisers Hyderabad "] = "Sunrisers Hyderabad"
PastTeamData$TeamName[PastTeamData$TeamName == "Deccan Chargers"] = "Sunrisers Hyderabad"
#Remove data for withdrawn player and coach

PastTeamData = PastTeamData[!trim(PastTeamData$Role) %in% c("withdrawn player","coach"),]

#Read in all Player Data
PlayersData = read.xlsx("PlayersData.xlsx",1,stringsAsFactors=F)



########################Functions to find of Total Runs,Total Wickets,Total Out and Total Overs######################
TeamWiseData =""
PlayersData$Year = as.integer(PlayersData$Year)
PastTeamData$Year = as.integer(PastTeamData$Year)

table(PlayersData$Year)
#1
n= 7
x=NULL
newdata=NULL
custom_function_totalruns <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year == df$Year-1  & 
                      PlayersData$format %in% c("ODI","T20I","TWENTY20"),c("PlayerName","format","Runs")]
  data$Runs = sapply(data$Runs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,Runs = sum(Runs),na.rm=TRUE)[2]
  newdata <- x[order(-x$Runs),] 
  sum(newdata[1:n],na.rm=TRUE)
}

TeamWiseRuns = ddply(PastTeamData, .(Year,TeamName),  custom_function_totalruns)
names(TeamWiseRuns)[3] = "Total Runs"
str(PlayersData)
#2
custom_function_totalwickets <- function(df) 
{
  #df1 = data.frame(df$TeamName,df$Year)
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year == df$Year-1  & 
                       PlayersData$format %in% c("ODI","T20I","TWENTY20"),c("PlayerName","format","Wickets")]
  data$Wickets = sapply(data$Wickets, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,Wickets = sum(Wickets),na.rm=TRUE)[2]
  newdata <- x[order(-x$Wickets),] 
  sum(newdata[1:n],na.rm=TRUE)
}


TeamWiseWickets = ddply(PastTeamData, .(Year,TeamName),  custom_function_totalwickets)
names(TeamWiseWickets)[3] = "Total Wickets"

#3
custom_function_totalouts <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year == df$Year-1  & 
                       PlayersData$format %in% c("ODI","T20I","TWENTY20"),c("PlayerName","format","Outs")]
  data$Outs = sapply(data$Outs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,Outs = sum(Outs),na.rm=TRUE)[2]
  newdata <- x[order(-x$Outs),] 
  sum(newdata[1:n],na.rm=TRUE)
}

TeamWiseOuts = ddply(PastTeamData, .(Year,TeamName),  custom_function_totalouts)
names(TeamWiseOuts)[3] = "Total Outs"

#4
custom_function_totalovers <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year == df$Year-1  & 
                       PlayersData$format %in% c("ODI","T20I","TWENTY20"),c("PlayerName","format","Overs")]
  data$Overs = sapply(data$Overs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,Overs = sum(Overs),na.rm=TRUE)[2]
  newdata <- x[order(-x$Overs),] 
  sum(newdata[1:n],na.rm=TRUE)
  
}
TeamWiseOvers = ddply(PastTeamData, .(Year,TeamName),  custom_function_totalovers)
names(TeamWiseOvers)[3] = "Total Overs"


########################Functions to find of Mean of Batting Ave,Bowl Ave,Batting S.R,Bowl S.R,Economy######################
#5
custom_function_totalBA <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year == df$Year-1 & PlayersData$format %in% c("ODI","T20I","TWENTY20"),c("PlayerName","format","Runs","Outs","Batting.Average")]
  
  data$Runs = sapply(data$Runs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Outs = sapply(data$Outs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,Batting.Average = ifelse(sum(Outs) > 0,sum(Runs,na.rm=TRUE)/sum(Outs,na.rm=TRUE),NA))[2]
  newdata <- x[order(-x$Batting.Average),] 
  mean(newdata[1:n],na.rm=TRUE)
  sum(newdata[1:n],na.rm=TRUE)/n
}
TeamWiseBA = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBA)
names(TeamWiseBA)[3] = "Batting Average"


#6
custom_function_totalBowlA <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year == df$Year-1   &   PlayersData$format %in% c("ODI","T20I","TWENTY20"),c("PlayerName","format","BowlRuns","Wickets","Bowling.Average")]
  data$BowlRuns = sapply(data$BowlRuns, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Wickets = sapply(data$Wickets, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,Bowling.Average = ifelse(sum(Wickets) > 0,sum(BowlRuns,na.rm=TRUE)/sum(Wickets,na.rm=TRUE),NA))[2]
  newdata <- x[order(-x$Bowling.Average),] 
  mean(newdata[1:7],na.rm=TRUE)
  sum(newdata[1:7],na.rm=TRUE)/7

}
TeamWiseBowlA = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBowlA)
names(TeamWiseBowlA)[3] = "Bowling Average"


#7
custom_function_totalBattingSR <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year == df$Year-1   & 
                                                                          PlayersData$format %in% c("ODI","T20I","TWENTY20"),c("PlayerName","format","Runs","Balls","Strike.Rate")]

  data$Runs = sapply(data$Runs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Balls = sapply(data$Balls, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,Strike.Rate = ifelse(sum(Balls) > 0,(sum(Runs,na.rm=TRUE)/sum(Balls,na.rm=TRUE))*100,NA))[2]
  newdata <- x[order(-x$Strike.Rate),] 
  mean(newdata[1:7],na.rm=TRUE)
  sum(newdata[1:7],na.rm=TRUE)/7
}


TeamWiseBattingSR = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBattingSR)
names(TeamWiseBattingSR)[3] = "Batting SR"

#8
custom_function_totalBowlingSR <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year == df$Year-1   &   PlayersData$format %in% c("ODI","T20I","TWENTY20"),c("PlayerName","format","Overs","Wickets","Bowl.SR")]
  data$Overs = sapply(data$Overs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Wickets = sapply(data$Wickets, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,Bowl.SR = ifelse(sum(Wickets) > 0,(sum(Overs*6,na.rm=TRUE)/sum(Wickets,na.rm=TRUE)),NA))[2]
  newdata <- x[order(-x$Bowl.SR),] 
  mean(newdata[1:7],na.rm=TRUE)
  sum(newdata[1:7],na.rm=TRUE)/7
}


TeamWiseBowlingSR = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBowlingSR)
names(TeamWiseBowlingSR)[3] = "Bowling SR"

#9
custom_function_totalEconomy <- function(df)
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year == df$Year-1 & PlayersData$format %in% c("ODI","T20I","TWENTY20"),c("PlayerName","format","BowlRuns","Overs","Economy")]
  data$BowlRuns = sapply(data$BowlRuns, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Overs = sapply(data$Overs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,Economy = ifelse(sum(Overs) > 0,(sum(BowlRuns,na.rm=TRUE)/sum(Overs,na.rm=TRUE)),NA))[2]
  newdata <- x[order(-x$Economy),] 
  mean(newdata[1:7],na.rm=TRUE)
  sum(newdata[1:7],na.rm=TRUE)/7
}


TeamWiseEconomy = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalEconomy)
names(TeamWiseEconomy)[3] = "Economy"
#10
AllRounders = subset(PastTeamData,trim(PastTeamData$PlayingRole) == "Playing role: Allrounder")

##Number of allrounders in each team
ar=ddply(AllRounders, .(Year,TeamName),"nrow") 

##Number of overseas bowlers
custom_function_OSbowl <- function(df) 
{
  nrow(df[df$Category=="Overseas" & grepl("Bowl",df$PlayingRole,ignore.case = TRUE),])
}

TeamWiseOSBowlers = ddply(PastTeamData, .(Year,TeamName),  custom_function_OSbowl)
names(TeamWiseOSBowlers)[3] = "OS Bowlers"

#10
AllRounders = subset(PastTeamData,trim(PastTeamData$PlayingRole) == "Playing role: Allrounder")

##Number of overseas bowlers
custom_function_OSbat <- function(df) 
{
  nrow(df[df$Category=="Overseas" & grepl("Bat",df$PlayingRole,ignore.case = TRUE),])
  
}

TeamWiseOSBat = ddply(PastTeamData, .(Year,TeamName),  custom_function_OSbat)
names(TeamWiseOSBat)[3] = "OS Batsmen"



TeamWiseData =""
TeamWiseData = merge(TeamWiseRuns,TeamWiseWickets,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseOuts,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseOvers,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseBA,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseBowlA,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseBattingSR,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseBowlingSR,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseEconomy,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseOSBowlers,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseOSBat,by=intersect(c("Year","TeamName"),c("Year","TeamName")))

str(TeamWiseData)
TeamWiseData$TS1 = TeamWiseData$'Batting Average'/TeamWiseData$'Bowling Average'
TeamWiseData$TS2 = TeamWiseData$'Batting SR'/TeamWiseData$'Bowling SR'
TeamWiseData$TS3 = TeamWiseData$TS1*TeamWiseData$TS2
TeamWiseData[,c("Year","TeamName","TS3")]


MatchResults1 = MatchResults[MatchResults$Team1Name != "Kochi",]
MatchResults1 = MatchResults1[MatchResults1$Team2Name != "Kochi",]
MatchResults1 = MatchResults1[MatchResults1$Team1Name != "Pune Warriors" ,]
MatchResults1 = MatchResults1[MatchResults1$Team2Name != "Pune Warriors" ,]

table(MatchResults1$Year)
#Matchwise - Difference in Total Runs
custom_diffruns = function(x)
{
  as.integer(TeamWiseData[TeamWiseData$TeamName == x[12] & TeamWiseData$Year == x[4],"Total Runs"]) - as.integer(TeamWiseData[TeamWiseData$TeamName == x[13] & TeamWiseData$Year == x[4],"Total Runs"])
}
MatchResults1$DiffRuns=apply(MatchResults1,1,custom_diffruns)

#Matchwise - Difference in Total Wickets
custom_diffwickets = function(x)
{
  as.integer(TeamWiseData[TeamWiseData$TeamName == x[12] & TeamWiseData$Year == x[4],"Total Wickets"]) - as.integer(TeamWiseData[TeamWiseData$TeamName == x[13] & TeamWiseData$Year == x[4],"Total Wickets"])
}
MatchResults1$DiffWickets=apply(MatchResults1,1,custom_diffwickets)

#Matchwise - Difference in Total Outs
custom_diffouts = function(x)
{
  as.integer(TeamWiseData[TeamWiseData$TeamName == x[12] & TeamWiseData$Year == x[4],"Total Outs"]) - as.integer(TeamWiseData[TeamWiseData$TeamName == x[13] & TeamWiseData$Year == x[4],"Total Outs"])
}
MatchResults1$DiffOuts=apply(MatchResults1,1,custom_diffouts)

#Matchwise - Difference in Total Overs
custom_diffovers = function(x)
{
  as.integer(TeamWiseData[TeamWiseData$TeamName == x[12] & TeamWiseData$Year == x[4],"Total Overs"]) - as.integer(TeamWiseData[TeamWiseData$TeamName == x[13] & TeamWiseData$Year == x[4],"Total Overs"])
}
MatchResults1$DiffOvers=apply(MatchResults1,1,custom_diffovers)

#Matchwise - Difference in Batting Average
custom_diffbatavg = function(x)
{
  as.numeric(TeamWiseData[TeamWiseData$TeamName == x[12] & TeamWiseData$Year == x[4],"Batting Average"]) - as.numeric(TeamWiseData[TeamWiseData$TeamName == x[13] & TeamWiseData$Year == x[4],"Batting Average"])
}
MatchResults1$AvgBat=apply(MatchResults1,1,custom_diffbatavg)

#Matchwise - Difference in Bowling Average
custom_diffbowlavg = function(x)
{
  as.numeric(TeamWiseData[TeamWiseData$TeamName == x[12] & TeamWiseData$Year == x[4],"Bowling Average"]) - as.numeric(TeamWiseData[TeamWiseData$TeamName == x[13] & TeamWiseData$Year == x[4],"Bowling Average"])
}
MatchResults1$AvgBowl=apply(MatchResults1,1,custom_diffbowlavg)

#Matchwise - Difference in Batting SR
custom_diffbattingSR = function(x)
{
  as.numeric(TeamWiseData[TeamWiseData$TeamName == x[12] & TeamWiseData$Year == x[4],"Batting SR"]) - as.numeric(TeamWiseData[TeamWiseData$TeamName == x[13] & TeamWiseData$Year == x[4],"Batting SR"])
}
MatchResults1$BattingSR=apply(MatchResults1,1,custom_diffbattingSR)


#Matchwise - Difference in Bowling SR
custom_diffbowlingSR = function(x)
{
  as.numeric(TeamWiseData[TeamWiseData$TeamName == x[12] & TeamWiseData$Year == x[4],"Bowling SR"]) - as.numeric(TeamWiseData[TeamWiseData$TeamName == x[13] & TeamWiseData$Year == x[4],"Bowling SR"])
}
MatchResults1$BowlingSR=apply(MatchResults1,1,custom_diffbowlingSR)

#Matchwise - Difference in Economy
custom_diffeconomy = function(x)
{
  as.numeric(TeamWiseData[TeamWiseData$TeamName == x[12] & TeamWiseData$Year == x[4],"Economy"]) - as.numeric(TeamWiseData[TeamWiseData$TeamName == x[13] & TeamWiseData$Year == x[4],"Economy"])
}
MatchResults1$Economy=apply(MatchResults1,1,custom_diffeconomy)

#Matchwise - Difference in number of OS Bowlers
custom_diffOSBowl = function(x)
{
  as.numeric(TeamWiseData[TeamWiseData$TeamName == x[12] & TeamWiseData$Year == x[4],"OS Bowlers"]) - as.numeric(TeamWiseData[TeamWiseData$TeamName == x[13] & TeamWiseData$Year == x[4],"OS Bowlers"])
}
MatchResults1$OSBowlers=apply(MatchResults1,1,custom_diffOSBowl)

#Matchwise - Difference in number of OS Batsmen
custom_diffOSBat = function(x)
{
  as.numeric(TeamWiseData[TeamWiseData$TeamName == x[12] & TeamWiseData$Year == x[4],"OS Batsmen"]) - as.numeric(TeamWiseData[TeamWiseData$TeamName == x[13] & TeamWiseData$Year == x[4],"OS Batsmen"])
}

MatchResults1$OSBat=apply(MatchResults1,1,custom_diffOSBat)

#Teamwise Strength Diff
custom_diffTS3 = function(x)
{
  as.numeric(TeamWiseData[TeamWiseData$TeamName == x[12] & TeamWiseData$Year == x[4],"TS3"]) - as.numeric(TeamWiseData[TeamWiseData$TeamName == x[13] & TeamWiseData$Year == x[4],"TS3"])
}

MatchResults1$TS3=apply(MatchResults1,1,custom_diffTS3)

str(MatchResults1)
#Adding the outcome varaible Team1 - Result and predictor variable - Team 1 Home Away

MatchResults1$Team1Result = ifelse(as.character(MatchResults1$Winner) == as.character(MatchResults1$Team.1), 
                                   1,0)

#Team 1 Ground = Home i.e 1
#Team 2 Ground = Away i.e 2
#OtherWise = Neutral i.e 3

MatchResults1$Team1HomeAway = ifelse(as.character(MatchResults1$Team.1) == as.character(MatchResults1$GroundCode),1,
       ifelse(as.character(MatchResults1$Team.2) == as.character(MatchResults1$GroundCode),2,3))

  



str(MatchResults1)
str(TeamWiseData)
MatchResultsData = MatchResults1[,c("Year","Team1Name","Team2Name","Ground","DiffRuns","DiffWickets",
                                    "DiffOuts","DiffOvers","AvgBat","AvgBowl",
                                    "BattingSR","BowlingSR","Economy","Team1HomeAway","Team1Result",
                                    "OSBowlers","OSBat","TS3")]

write.xlsx(MatchResultsData,"MatchResultsModelData1.xls")
