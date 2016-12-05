trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

PlayersData = read.xlsx("PlayersData.xlsx",1,stringsAsFactors=FALSE)
str(PlayersData)
unique(PlayersData$PlayerName)
players = read.xlsx("PastTeamData.xls", sheetName = "IPL Team Data")
# Remove the withdrawn players from the player list 
players = players[!trim(players$Role) %in% c("coach","withdrawn player"),]


Names = players[!players$Name %in% unique(PlayersData$PlayerName),"Name"]


PlayersData[PlayersData$PlayerName== "Shiv Chanderpaul",]

unique(PlayersData$PlayerName)

subset(PlayersData,PlayerName == "Kane Richardson")

PlayerNames = c("KV Sharma","S Chanderpaul","U Chand","TG Southee","K Cooper","BRM Taylor","TA Boult","KS Williamson","LR Shukla","Gurkeerat  Singh","Sean Abbott","Adam Milne")

i=1
while(i <=length(MissingPlayers))
{
  
  PlayerInfo.table = OverallPlayer.Info(PlayerNames[i])
  if(PlayerInfo.table != 0)
  {
    if(i == 1)
    {
      AllPlayerInfo = PlayerInfo.table
    }else
    {
      AllPlayerInfo = rbind(AllPlayerInfo ,PlayerInfo.table)
    } 
  }
  i = i+1
}

PastTeamData[PastTeamData$Year == "2014",]
IPLMatchResults[IPLMatchResults$Year == "2008",]


data = read.xlsx("PlayersData.xlsx",1,startRow=1,endRow=10)

head(PlayersData)
