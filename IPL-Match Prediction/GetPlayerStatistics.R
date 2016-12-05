library(XML)
##Scrubbing URL file
require(xlsx)

OverallPlayer.Info = function(playername)
{
  fileurl = paste0("http://www.cricmetric.com/playerstats.py?player=",playername)
  #fileurl = paste0("http://www.cricmetric.com/playerstats.py?player=",PlayerNames[401])
  fileurl = paste0(fileurl,"&format=all&role=all")
  doc <- htmlParse(fileurl)
  tableNodes = getNodeSet(doc,"//table")
  Bowling.Info=NULL
  Batting.Info=NULL
  Player.Info=NULL
  OverallPlayer.Info=NULL
  nrows = length(tableNodes)
  if(nrows == 0)
  {
    return(0)
  }
  else
  {
    j=1
    while(j <= nrows)
    {
    Player.Info <-  readHTMLTable(tableNodes[[j]],
                                 header=T, 
                                 which=1,
                                 stringsAsFactors=F)
    
    
    subDoc = xmlDoc(tableNodes[[j]])
    hrefs <- xpathSApply(subDoc, "//table[@class = 'scoretable']/tr/td/a", xmlGetAttr, 'href')
    hrefs
    #hrefs = xpathSApply(subDoc, "//table[@class = 'scoretable']/tr/td//a[contains(@href, 'ODI')]",xmlValue)
    if(length(grep("Test",hrefs))) {
    Player.Info$format  = "Test"
    } else if (length(grep("ODI",hrefs))) {
    Player.Info$format  = "ODI"
    } else if (length(grep("T20I",hrefs))) {
    Player.Info$format  = "T20I"
    } else if(length(grep("TWENTY20",hrefs)))
    Player.Info$format  = "TWENTY20"
    
    Player.Info = Player.Info[!Player.Info$Year=="Total",]
    if(names(Player.Info)[6] == "Bowling Average")
    {
      type = "Bowling"
      names(Player.Info)[3] = "BowlRuns"
      names(Player.Info)[7] = "Bowl SR"
    }else
    {
      type = "Batting"
    }
    
    length(Batting.Info)
    
    #exists("Bowling.Info")
    #exists("Player.Info")
    if(type == "Bowling")
    {
      if( length(Bowling.Info) > 0)
      {
        Bowling.Info = rbind(Bowling.Info,Player.Info)
      }else
      {
        Bowling.Info = Player.Info
      }
    }else 
    {
      if(length(Batting.Info) > 0)
      {
        Batting.Info = rbind(Batting.Info,Player.Info)
      }else
      {
        Batting.Info = Player.Info
      } 
     
    }
    j= j+1
  }
  
  nrow(Batting.Info) == NULL
  nrow(Bowling.Info) == NULL
  
  class(Batting.Info)
  
  if(length(Batting.Info) > 0 & length(Bowling.Info) >0)
  {
    OverallPlayer.Info = merge(Batting.Info,Bowling.Info,by.x = c("Year","format"),by.y = c("Year","format"),all.x=TRUE,all.y=TRUE)
    OverallPlayer.Info$PlayerName = playername
    return(OverallPlayer.Info)
  }
  
  if(length(Batting.Info) > 0)
  {
    OverallPlayer.Info = cbind(Batting.Info,Overs="",BowlRuns="",Wickets="",Economy="",'Bowling Average'="",'Bowl SR'="")
    OverallPlayer.Info$PlayerName = playername
    return(OverallPlayer.Info)
  }
  
  if(length(Bowling.Info) >0)
  {
    
    OverallPlayer.Info = cbind(Runs="",Balls="",Outs="",'Strike Rate'="",'Batting Average'="",Bowling.Info)
    OverallPlayer.Info$PlayerName = playername
    return(OverallPlayer.Info)
  }

  return(0)
  }
}
  
  
players = read.xlsx("PastTeamData.xls", sheetName = "IPL Team Data")

str(players)
PlayerNames = table(players[,c("Name")])
str(PlayerNames)

PlayerNames2 = table(players[,c("Name","Category")])

PlayerNames2[PlayerNames2[,1] > 0 & PlayerNames2[,2] >0,]




PlayerNames = unique(players[,c("Name","Category","PlayingRole","BattingStyle","BowlingStyle")])

Domestic = PlayerNames[PlayerNames$Category == "Domestic",]
Overseas = PlayerNames[PlayerNames$Category == "Overseas",]

merge(PlayerNames,PlayerNames2)



i=1
AllPlayerInfo=NULL
while(i <=510)
{

  PlayerInfo.table = OverallPlayer.Info(PlayerNames[i])
  if(PlayerInfo.table != 0)
  {
    if(i == 401)
    {
      AllPlayerInfo = PlayerInfo.table
    }else
    {
      AllPlayerInfo = rbind(AllPlayerInfo ,PlayerInfo.table)
    } 
  }
  i = i+1
}
head(AllPlayerInfo)
names(PlayerInfo.table)

PlayerNames[398]
write.xlsx(AllPlayerInfo,"PlayerData6.xls",sheetName="2015 IPL Player Data")
 
