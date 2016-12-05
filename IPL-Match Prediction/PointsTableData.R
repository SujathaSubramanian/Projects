library(plyr)
#Run 1
#Top 7 Players in each time
#Pick data for last 1 year
#Collect data for ODI,T20I and Twenty20 separately

########################Functions to find of Total Runs,Total Wickets,Total Out and Total Overs######################
TeamWiseData =""
PlayersData$Year = as.integer(PlayersData$Year)
PastTeamData$Year = as.integer(PastTeamData$Year)
n= 10
x=NULL
newdata=NULL

#1 - Runs

custom_function_totalrunsODI <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)  & 
                       PlayersData$format %in% c("ODI"),c("PlayerName","format","Runs")]
  data$Runs = sapply(data$Runs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,ODIRuns = sum(Runs),na.rm=TRUE)[2]
  newdata <- x[order(-x$ODIRuns),] 
  sum(newdata[1:n],na.rm=TRUE)
}


TeamWiseRunsODI = ddply(PastTeamData, .(Year,TeamName),  custom_function_totalrunsODI)

names(TeamWiseRunsODI)[3] = "Total ODI Runs"


custom_function_totalrunsT20I <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year  %in% c(df$Year-1,df$Year-2,df$Year-3)  & 
                       PlayersData$format %in% c("T20I"),c("PlayerName","format","Runs")]
  data$Runs = sapply(data$Runs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,T20IRuns = sum(Runs),na.rm=TRUE)[2]
  newdata <- x[order(-x$T20IRuns),] 
  sum(newdata[1:n],na.rm=TRUE)
}


TeamWiseRunsT20I = ddply(PastTeamData, .(Year,TeamName),  custom_function_totalrunsT20I)

names(TeamWiseRunsT20I)[3] = "Total T20I Runs"


custom_function_totalrunsTWENTY20 <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)  & 
                       PlayersData$format %in% c("TWENTY20"),c("PlayerName","format","Runs")]
  data$Runs = sapply(data$Runs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,TWENTY20Runs = sum(Runs),na.rm=TRUE)[2]
  if(nrow(x) > 0)
  {
  newdata <- x[order(-x$TWENTY20Runs),] 
  sum(newdata[1:n],na.rm=TRUE)
  }
}


TeamWiseRunsTwenty20 = ddply(PastTeamData, .(Year,TeamName),  custom_function_totalrunsTWENTY20)

names(TeamWiseRunsTwenty20)[3] = "Total TWENTY20 Runs"


#2 - Wickets

custom_function_totalwicketsODI <- function(df) 
{
  #df1 = data.frame(df$TeamName,df$Year)
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)  & 
                       PlayersData$format %in% c("ODI"),c("PlayerName","format","Wickets")]
  data$Wickets = sapply(data$Wickets, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,ODIWickets = sum(Wickets),na.rm=TRUE)[2]
  newdata <- x[order(-x$ODIWickets),] 
  sum(newdata[1:n],na.rm=TRUE)
}


TeamWiseWicketsODI = ddply(PastTeamData, .(Year,TeamName),  custom_function_totalwicketsODI)
names(TeamWiseWicketsODI)[3] = "Total ODI Wickets"

custom_function_totalwicketsT20I <- function(df) 
{
  #df1 = data.frame(df$TeamName,df$Year)
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)  & 
                       PlayersData$format %in% c("T20I"),c("PlayerName","format","Wickets")]
  data$Wickets = sapply(data$Wickets, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,T20IWickets = sum(Wickets),na.rm=TRUE)[2]
  newdata <- x[order(-x$T20IWickets),] 
  sum(newdata[1:n],na.rm=TRUE)
}


TeamWiseWicketsT20I = ddply(PastTeamData, .(Year,TeamName),  custom_function_totalwicketsT20I)
names(TeamWiseWicketsT20I)[3] = "Total T20I Wickets"

custom_function_totalwicketsTWENTY20 <- function(df) 
{
  #df1 = data.frame(df$TeamName,df$Year)
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)  & 
                       PlayersData$format %in% c("TWENTY20"),c("PlayerName","format","Wickets")]
  data$Wickets = sapply(data$Wickets, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,TWENTY20Wickets = sum(Wickets),na.rm=TRUE)[2]
  if(nrow(x) > 0)
  {
  newdata <- x[order(-x$TWENTY20Wickets),] 
  sum(newdata[1:n],na.rm=TRUE)
  }
}


TeamWiseWicketsTwenty20 = ddply(PastTeamData, .(Year,TeamName),  custom_function_totalwicketsTWENTY20)
names(TeamWiseWicketsTwenty20)[3] = "Total TWENTY20 Wickets"


#3 Outs
custom_function_totaloutsODI <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)  & 
                       PlayersData$format %in% c("ODI"),c("PlayerName","format","Outs")]
  data$Outs = sapply(data$Outs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,ODIOuts = sum(Outs),na.rm=TRUE)[2]
  newdata <- x[order(-x$ODIOuts),] 
  sum(newdata[1:n],na.rm=TRUE)
}

TeamWiseOutsODI = ddply(PastTeamData, .(Year,TeamName),  custom_function_totaloutsODI)
names(TeamWiseOutsODI)[3] = "Total ODI Outs"

custom_function_totaloutsT20I <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)  & 
                       PlayersData$format %in% c("T20I"),c("PlayerName","format","Outs")]
  data$Outs = sapply(data$Outs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,T20IOuts = sum(Outs),na.rm=TRUE)[2]
  newdata <- x[order(-x$T20IOuts),] 
  sum(newdata[1:n],na.rm=TRUE)
}

TeamWiseOutsT20I = ddply(PastTeamData, .(Year,TeamName),  custom_function_totaloutsT20I)
names(TeamWiseOutsT20I)[3] = "Total T20I Outs"

custom_function_totaloutsTWENTY20 <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)  & 
                       PlayersData$format %in% c("TWENTY20"),c("PlayerName","format","Outs")]
  data$Outs = sapply(data$Outs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,TWENTY20Outs = sum(Outs),na.rm=TRUE)[2]
  if(nrow(x) > 0)
  {
  newdata <- x[order(-x$TWENTY20Outs),] 
  sum(newdata[1:n],na.rm=TRUE)
  }
}

TeamWiseOutsTWENTY20 = ddply(PastTeamData, .(Year,TeamName),  custom_function_totaloutsTWENTY20)
names(TeamWiseOutsTWENTY20)[3] = "Total TWENTY20 Outs"



#4 Overs
custom_function_totaloversODI <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)  & 
                       PlayersData$format %in% c("ODI"),c("PlayerName","format","Overs")]
  data$Overs = sapply(data$Overs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,ODIOvers = sum(Overs),na.rm=TRUE)[2]
  newdata <- x[order(-x$ODIOvers),] 
  sum(newdata[1:n],na.rm=TRUE)
  
}
TeamWiseOversODI = ddply(PastTeamData, .(Year,TeamName),  custom_function_totaloversODI)
names(TeamWiseOversODI)[3] = "Total ODI Overs"

custom_function_totaloversT20I <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)  & 
                       PlayersData$format %in% c("T20I"),c("PlayerName","format","Overs")]
  data$Overs = sapply(data$Overs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,T20IOvers = sum(Overs),na.rm=TRUE)[2]
  newdata <- x[order(-x$T20IOvers),] 
  sum(newdata[1:n],na.rm=TRUE)
  
}
TeamWiseOversT20I = ddply(PastTeamData, .(Year,TeamName),  custom_function_totaloversT20I)
names(TeamWiseOversT20I)[3] = "Total T20I Overs"

custom_function_totaloversTWENTY20 <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)  & 
                       PlayersData$format %in% c("TWENTY20"),c("PlayerName","format","Overs")]
  data$Overs = sapply(data$Overs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x= ddply(data,.(PlayerName),summarize,TWENTY20Overs = sum(Overs),na.rm=TRUE)[2]
  if(nrow(x) >0)
  {
    newdata <- x[order(-x$TWENTY20Overs),] 
    sum(newdata[1:n],na.rm=TRUE)
    
  }
}
TeamWiseOversTWENTY20 = ddply(PastTeamData, .(Year,TeamName),  custom_function_totaloversTWENTY20)
names(TeamWiseOversTWENTY20)[3] = "Total TWENTY20 Overs"


########################Functions to find of Mean of Batting Ave,Bowl Ave,Batting S.R,Bowl S.R,Economy######################
#5
custom_function_totalBAODI <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3) & PlayersData$format %in% c("ODI"),c("PlayerName","format","Runs","Outs","Batting.Average")]
  
  data$Runs = sapply(data$Runs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Outs = sapply(data$Outs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalRuns = sum(Runs,na.rm=TRUE),TotalOuts=sum(Outs,na.rm=TRUE))
  
  newdata <- x[order(-x$TotalRuns),] 
  sum(newdata[1:n,"TotalRuns"],na.rm=TRUE)/sum(newdata[1:n,"TotalOuts"],na.rm=TRUE)
  
}

TeamWiseBAODI = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBAODI)
names(TeamWiseBAODI)[3] = "BA ODI"

custom_function_totalBAT20I <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3) & PlayersData$format %in% c("T20I"),c("PlayerName","format","Runs","Outs","Batting.Average")]
  
  data$Runs = sapply(data$Runs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Outs = sapply(data$Outs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalRuns = sum(Runs,na.rm=TRUE),TotalOuts=sum(Outs,na.rm=TRUE))
  
  newdata <- x[order(-x$TotalRuns),] 
  sum(newdata[1:n,"TotalRuns"],na.rm=TRUE)/sum(newdata[1:n,"TotalOuts"],na.rm=TRUE)
  
}

TeamWiseBAT20I = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBAT20I)
names(TeamWiseBAT20I)[3] = "BA T20I"

custom_function_totalBATwenty20 <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3) & PlayersData$format %in% c("TWENTY20"),c("PlayerName","format","Runs","Outs","Batting.Average")]
  
  data$Runs = sapply(data$Runs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Outs = sapply(data$Outs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalRuns = sum(Runs,na.rm=TRUE),TotalOuts=sum(Outs,na.rm=TRUE))
  
  if(nrow(x) > 0)
  {
  newdata <- x[order(-x$TotalRuns),] 
  sum(newdata[1:n,"TotalRuns"],na.rm=TRUE)/sum(newdata[1:n,"TotalOuts"],na.rm=TRUE)
  }
}

TeamWiseBATwenty20 = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBATwenty20)
names(TeamWiseBATwenty20)[3] = "BA Twenty20"

#6 Bowling Average
custom_function_totalBowlAODI <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)   &   PlayersData$format %in% c("ODI"),c("PlayerName","format","BowlRuns","Wickets","Bowling.Average")]
  data$BowlRuns = sapply(data$BowlRuns, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Wickets = sapply(data$Wickets, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalBowlRuns = sum(BowlRuns,na.rm=TRUE),TotalWickets=sum(Wickets,na.rm=TRUE))
  newdata <- x[order(-x$TotalWickets),] 
  sum(newdata[1:n,"TotalBowlRuns"],na.rm=TRUE)/sum(newdata[1:n,"TotalWickets"],na.rm=TRUE)

}
TeamWiseBowlAODI = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBowlAODI)
names(TeamWiseBowlAODI)[3] = "BowlA ODI"

custom_function_totalBowlAT20I <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)   &   PlayersData$format %in% c("T20I"),c("PlayerName","format","BowlRuns","Wickets","Bowling.Average")]
  data$BowlRuns = sapply(data$BowlRuns, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Wickets = sapply(data$Wickets, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalBowlRuns = sum(BowlRuns,na.rm=TRUE),TotalWickets=sum(Wickets,na.rm=TRUE))
  newdata <- x[order(-x$TotalWickets),] 
  sum(newdata[1:n,"TotalBowlRuns"],na.rm=TRUE)/sum(newdata[1:n,"TotalWickets"],na.rm=TRUE)
  
  
}
TeamWiseBowlAT20I = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBowlAT20I)
names(TeamWiseBowlAT20I)[3] = "BowlA T20I"

custom_function_totalBowlATWENTY20 <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)   &   PlayersData$format %in% c("TWENTY20"),c("PlayerName","format","BowlRuns","Wickets","Bowling.Average")]
  data$BowlRuns = sapply(data$BowlRuns, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Wickets = sapply(data$Wickets, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalBowlRuns = sum(BowlRuns,na.rm=TRUE),TotalWickets=sum(Wickets,na.rm=TRUE))
  if(nrow(x) > 0)
  {
  newdata <- x[order(-x$TotalWickets),] 
  sum(newdata[1:n,"TotalBowlRuns"],na.rm=TRUE)/sum(newdata[1:n,"TotalWickets"],na.rm=TRUE)
  }
  
}
TeamWiseBowlATWENTY20 = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBowlATWENTY20)
names(TeamWiseBowlATWENTY20)[3] = "BowlA TWENTY20"


#7 Batting Strike Rate
custom_function_totalBattingSRODI <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)   & 
                       PlayersData$format %in% c("ODI"),c("PlayerName","format","Runs","Balls","Strike.Rate")]
  
  data$Runs = sapply(data$Runs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Balls = sapply(data$Balls, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalRuns = sum(Runs,na.rm=TRUE),TotalBalls=sum(Balls,na.rm=TRUE))
  newdata <- x[order(-x$TotalRuns),] 
  sum(newdata[1:n,"TotalRuns"],na.rm=TRUE)/sum(newdata[1:n,"TotalBalls"],na.rm=TRUE)*100
  
}


TeamWiseBattingSRODI = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBattingSRODI)
names(TeamWiseBattingSRODI)[3] = "BSR ODI"

custom_function_totalBattingSRT20I <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)   & 
                       PlayersData$format %in% c("T20I"),c("PlayerName","format","Runs","Balls","Strike.Rate")]
  
  data$Runs = sapply(data$Runs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Balls = sapply(data$Balls, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalRuns = sum(Runs,na.rm=TRUE),TotalBalls=sum(Balls,na.rm=TRUE))
  newdata <- x[order(-x$TotalRuns),] 
  sum(newdata[1:n,"TotalRuns"],na.rm=TRUE)/sum(newdata[1:n,"TotalBalls"],na.rm=TRUE)*100
  
}


TeamWiseBattingSRT20I = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBattingSRT20I)
names(TeamWiseBattingSRT20I)[3] = "BSR T20I"

custom_function_totalBattingSRTWENTY20 <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)   & 
                       PlayersData$format %in% c("TWENTY20"),c("PlayerName","format","Runs","Balls","Strike.Rate")]
  
  data$Runs = sapply(data$Runs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Balls = sapply(data$Balls, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalRuns = sum(Runs,na.rm=TRUE),TotalBalls=sum(Balls,na.rm=TRUE))
  if(nrow(x) > 0)
  {
  newdata <- x[order(-x$TotalRuns),] 
  sum(newdata[1:n,"TotalRuns"],na.rm=TRUE)/sum(newdata[1:n,"TotalBalls"],na.rm=TRUE)*100
  }
}


TeamWiseBattingSRTWENTY20 = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBattingSRTWENTY20)
names(TeamWiseBattingSRTWENTY20)[3] = "BSR TWENTY20"


#8 Bowling SR
custom_function_totalBowlSRODI <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)   &   PlayersData$format %in% c("ODI"),c("PlayerName","format","Overs","Wickets","Bowl.SR")]
  data$Overs = sapply(data$Overs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Wickets = sapply(data$Wickets, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalOvers = sum(Overs*6,na.rm=TRUE),TotalWickets= sum(Wickets,na.rm=TRUE))
  newdata <- x[order(-x$TotalWickets),] 
  sum(newdata[1:n,"TotalOvers"],na.rm=TRUE)/sum(newdata[1:n,"TotalWickets"],na.rm=TRUE)
  
}


TeamWiseBowlingSRODI = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBowlSRODI)
names(TeamWiseBowlingSRODI)[3] = "BowlSR ODI"

custom_function_totalBowlSRT20 <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)   &   PlayersData$format %in% c("T20I"),c("PlayerName","format","Overs","Wickets","Bowl.SR")]
  data$Overs = sapply(data$Overs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Wickets = sapply(data$Wickets, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalOvers = sum(Overs*6,na.rm=TRUE),TotalWickets= sum(Wickets,na.rm=TRUE))
  newdata <- x[order(-x$TotalWickets),] 
  sum(newdata[1:n,"TotalOvers"],na.rm=TRUE)/sum(newdata[1:n,"TotalWickets"],na.rm=TRUE)
  
}


TeamWiseBowlingSRT20 = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBowlSRT20)
names(TeamWiseBowlingSRT20)[3] = "BowlSR T20I"

custom_function_totalBowlSRTWENTY20 <- function(df) 
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3)   &   PlayersData$format %in% c("TWENTY20"),c("PlayerName","format","Overs","Wickets","Bowl.SR")]
  data$Overs = sapply(data$Overs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Wickets = sapply(data$Wickets, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalOvers = sum(Overs*6,na.rm=TRUE),TotalWickets= sum(Wickets,na.rm=TRUE))
  if(nrow(x) > 0)
  {
  newdata <- x[order(-x$TotalWickets),] 
  sum(newdata[1:n,"TotalOvers"],na.rm=TRUE)/sum(newdata[1:n,"TotalWickets"],na.rm=TRUE)
  }
}


TeamWiseBowlSRTWENTY20 = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalBowlSRTWENTY20)
names(TeamWiseBowlSRTWENTY20)[3] = "BowlSR TWENTY20"


#9 Economy
custom_function_totalEconomyODI <- function(df)
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3) & PlayersData$format %in% c("ODI"),c("PlayerName","format","BowlRuns","Overs","Economy")]
  data$BowlRuns = sapply(data$BowlRuns, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Overs = sapply(data$Overs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalBowlRuns = sum(BowlRuns,na.rm=TRUE),TotalOvers= sum(Overs,na.rm=TRUE))
  newdata <- x[order(-x$TotalOvers),] 
  sum(newdata[1:n,"TotalBowlRuns"],na.rm=TRUE)/sum(newdata[1:n,"TotalOvers"],na.rm=TRUE)
}


TeamWiseEconomyODI = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalEconomyODI)
names(TeamWiseEconomyODI)[3] = "Economy ODI"

custom_function_totalEconomyT20I <- function(df)
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3) & PlayersData$format %in% c("T20I"),c("PlayerName","format","BowlRuns","Overs","Economy")]
  data$BowlRuns = sapply(data$BowlRuns, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Overs = sapply(data$Overs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalBowlRuns = sum(BowlRuns,na.rm=TRUE),TotalOvers= sum(Overs,na.rm=TRUE))
  newdata <- x[order(-x$TotalOvers),] 
  sum(newdata[1:n,"TotalBowlRuns"],na.rm=TRUE)/sum(newdata[1:n,"TotalOvers"],na.rm=TRUE)
}
TeamWiseEconomyT20I = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalEconomyT20I)
names(TeamWiseEconomyT20I)[3] = "Economy T20I"

custom_function_totalEconomyTWENTY20 <- function(df)
{
  data = PlayersData[PlayersData$PlayerName %in% df$Name & PlayersData$Year %in% c(df$Year-1,df$Year-2,df$Year-3) & PlayersData$format %in% c("TWENTY20"),c("PlayerName","format","BowlRuns","Overs","Economy")]
  data$BowlRuns = sapply(data$BowlRuns, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  data$Overs = sapply(data$Overs, function(v) {as.numeric(gsub("\\,","", as.character(v)))})
  x=ddply(data,.(PlayerName),summarize,TotalBowlRuns = sum(BowlRuns,na.rm=TRUE),TotalOvers= sum(Overs,na.rm=TRUE))
  if(nrow(x) > 0)
  {
  newdata <- x[order(-x$TotalOvers),] 
  sum(newdata[1:n,"TotalBowlRuns"],na.rm=TRUE)/sum(newdata[1:n,"TotalOvers"],na.rm=TRUE)
  }
}


TeamWiseEconomyTWENTY20 = ddply(PastTeamData, .(Year,TeamName),   custom_function_totalEconomyTWENTY20)
names(TeamWiseEconomyTWENTY20)[3] = "Economy TWENTY20"



#10 Number of overseas bowlers
custom_function_OSbowl <- function(df) 
{
  nrow(df[df$Category=="Overseas" & grepl("Bowl",df$PlayingRole,ignore.case = TRUE),])
}

TeamWiseOSBowlers = ddply(PastTeamData, .(Year,TeamName),  custom_function_OSbowl)
names(TeamWiseOSBowlers)[3] = "OS Bowlers"

#11 Number of overseas bat

##Number of overseas bowlers
custom_function_OSbat <- function(df) 
{
  nrow(df[df$Category=="Overseas" & grepl("Bat",df$PlayingRole,ignore.case = TRUE),])
  
}

TeamWiseOSBat = ddply(PastTeamData, .(Year,TeamName),  custom_function_OSbat)
names(TeamWiseOSBat)[3] = "OS Batsmen"

#12 Number for International rank players



############################################Prepare final data set by merging all info ######################
TeamWiseData =""
TeamWiseData = merge(TeamWiseRunsODI,TeamWiseRunsT20I,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseRunsTwenty20,by=intersect(c("Year","TeamName"),c("Year","TeamName")))

TeamWiseData = merge(TeamWiseData,TeamWiseWicketsODI,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseWicketsT20I,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseWicketsTwenty20,by=intersect(c("Year","TeamName"),c("Year","TeamName")))

TeamWiseData = merge(TeamWiseData,TeamWiseOutsODI,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseOutsT20I,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseOutsTWENTY20,by=intersect(c("Year","TeamName"),c("Year","TeamName")))

TeamWiseData = merge(TeamWiseData,TeamWiseOversODI,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseOversT20I,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseOversTWENTY20,by=intersect(c("Year","TeamName"),c("Year","TeamName")))

TeamWiseData = merge(TeamWiseData,TeamWiseBAODI,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseBAT20I,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseBATwenty20,by=intersect(c("Year","TeamName"),c("Year","TeamName")))

TeamWiseData = merge(TeamWiseData,TeamWiseBowlAODI,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseBowlAT20I,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseBowlATWENTY20,by=intersect(c("Year","TeamName"),c("Year","TeamName")))


TeamWiseData = merge(TeamWiseData,TeamWiseBattingSRODI,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseBattingSRT20I,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseBattingSRTWENTY20,by=intersect(c("Year","TeamName"),c("Year","TeamName")))

TeamWiseData = merge(TeamWiseData,TeamWiseBowlingSRODI,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseBowlingSRT20,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseBowlSRTWENTY20,by=intersect(c("Year","TeamName"),c("Year","TeamName")))

TeamWiseData = merge(TeamWiseData,TeamWiseEconomyODI,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseEconomyT20I,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseEconomyTWENTY20,by=intersect(c("Year","TeamName"),c("Year","TeamName")))

TeamWiseData = merge(TeamWiseData,TeamWiseOSBowlers,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
TeamWiseData = merge(TeamWiseData,TeamWiseOSBat,by=intersect(c("Year","TeamName"),c("Year","TeamName")))

str(TeamWiseData)
#TeamWiseData$TS1 = TeamWiseData$'Batting Average'/TeamWiseData$'Bowling Average'
#TeamWiseData$TS2 = TeamWiseData$'Batting SR'/TeamWiseData$'Bowling SR'
#TeamWiseData$TS3 = TeamWiseData$TS1*TeamWiseData$TS2

#TeamWiseData[,c("Year","TeamName","TS1")]
#TeamWiseData[,c("Year","TeamName","TS2")]
#TeamWiseData[,c("Year","TeamName","TS3")]


Points = read.xlsx("PointsTableData.xls",1)

Points = Points[,c("Year","TeamName","Points")]

TeamWiseData = merge(TeamWiseData,Points,all = TRUE,by=intersect(c("Year","TeamName"),c("Year","TeamName")))
write.xlsx(TeamWiseData,"PointsTableData.xls")


