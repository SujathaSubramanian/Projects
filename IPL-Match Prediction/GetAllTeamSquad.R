library(XML)
##Scrubbing URL file
require(xlsx)

##
## Function scrubs the information about IPL teams. 
## 

Year =players[i,1]
TeamName=as.character(players[i,2])
fileurl=players[i,3]
playercount=players[i,4] 
tableindex=players[i,5])

Team.Info = function(Year,TeamName,fileurl,playercount,tableindex)
{
  
  doc <- htmlParse(fileurl)
  doc
  
  tableNodes = getNodeSet(doc,"//table")
  
  #length(tableNodes)
  #if(tableNodes )
  
  PlayerDF = data.frame("Year"="","TeamName"="","Name"="","Role"="",
                      "Age"="","PlayingRole"="","BattingStyle"="","BowlingStyle"="",stringsAsFactors=FALSE)
                       
  j=1
  newstr = ""
  for(j in 1:playercount)
  {
    
    Player.Info <-  readHTMLTable(tableNodes[[tableindex+j]],
                                      header=F, 
                                      which=1,
                                      stringsAsFactors=F)
    newstr = ""
    str=""
    
    str=  Player.Info$V2[1] 
    str = gsub("\t|\r","",str)
    str = unlist(strsplit(str,"\n"))
    str = gsub(" *$", "", str)
    str = gsub("*^ ", "", str)
    str = str[nchar(str)>0]
    
    newstr[1] = str[1]
    n= grep("captain|wicketkeeper|coach|withdrawn player",str)
    
    if(length(n) >0 )
      newstr[2] = str[n]
    
    n = grep("Age",str)
    if(length(n) >0 )
      newstr[3] = str[n]
    
    n = grep("Playing role",str)
    if(length(n) >0 )
      newstr[4] = str[n]
    
    
    n= grep("Batting",str)
    if(length(n) >0 )
      newstr[5] = str[n]
    
    n= grep("Bowling",str)
    if(length(n) >0 )
      newstr[6] = str[n]
    
    
    
    df=c(Year,TeamName,newstr[1:6])
    
    PlayerDF= cbind(rbind(PlayerDF,df))
    
    j=j+1
  }
return(PlayerDF)
}

##
## Function scrubs the information about IPL teams played in year 2011. 
## 
Team2011.Info = function(Year,TeamName,fileurl)
{
  doc <- htmlParse(fileurl)
  
  
  names <- xpathSApply(doc, "//section[@class='ch-players module hide-portrait hide-for-mob']", xmlValue) 
  
  PlayerDF = data.frame("Year"="","TeamName"="","Name"="","Role"="",
                        "Age"="","PlayingRole"="","BattingStyle"="","BowlingStyle"="",stringsAsFactors=FALSE)
  
  str = gsub("\t|\r","",names)
  
  #Split the string based on newline
  str = unlist(strsplit(str,"\n"))
  #Remove the trailing spaces
  str = gsub(" *$", "", str)
  #Remove the leading spaces
  str = gsub("^ *","",str)  
  #Remove all empty string
  str = str[nchar(str)>0]
  str=str[-1]
  
  
  newstr = ""
  for(j in 1:length(str))
  {
    
    df=c(Year,TeamName,str[j],"","","","","")
    
    PlayerDF= cbind(rbind(PlayerDF,df))
    
    j=j+1
  }
  return(PlayerDF)
}


fileurl = "http://www.espncricinfo.com/indian-premier-league-2014/content/squad/735917.html"

Team2014.Info = function(Year,TeamName,fileurl)
{
  doc <- htmlParse(fileurl)
  
  
  names <- xpathSApply(doc, "//div[@class='large-13 medium-13 small-13 columns']/h3/a", xmlValue) 
  PlayerInfo <- xpathSApply(doc, "//div[@class='large-13 medium-13 small-13 columns']/span", xmlValue) 
  
  
  str = gsub("\t|\r","",names)
  
  #Split the string based on newline
  str = unlist(strsplit(str,"\n",fixed=TRUE))
  #Remove the trailing spaces
  str = gsub(" *$", "", str)
  #Remove the leading spaces
  str = gsub("^ *","",str)  
  #Remove all empty string
  str = str[nchar(str)>0]
  
  
  newstr = ""
  i=1
  playerIndex= 0
  
  
  PlayerDF = data.frame("Year"="","TeamName"="","Name"="","Role"="",
                        "Age"="","PlayingRole"="","BattingStyle"="","BowlingStyle"="",stringsAsFactors=FALSE)
  
  
  for(i in 1:length(PlayerInfo))
  {
    
    
    if(grepl("Age",PlayerInfo[i]))
    {
      PlayerDF=rbind(PlayerDF,c(Year,TeamName,str[playerIndex+1],"","","","",""))
      if(i==1){PlayerDF = PlayerDF[-1,]}
      playerIndex= playerIndex +1
      PlayerDF[playerIndex,"Age"] = PlayerInfo[i] 
    }
    
    if(grepl("role",PlayerInfo[i]))
    {
      PlayerDF[playerIndex,"PlayingRole"] = PlayerInfo[i] 
    }
    
    if(grepl("Batting",PlayerInfo[i]))
    {
      PlayerDF[playerIndex,"BattingStyle"] = PlayerInfo[i] 
    }
    
    if(grepl("Bowling",PlayerInfo[i]))
    {
      PlayerDF[playerIndex,"BowlingStyle"] = PlayerInfo[i] 
    }
    
  }
  return(PlayerDF)
}
  

##
##Read all the data in the ESPN links spreadsheet and get the information about the squad.
##
players = read.xlsx("ESPNLinks.xlsx",1)
i=59
first=""
while (i<= nrow(players))
{

  if(players[i,6] == "No")
  {
    if(first != "FALSE")
      first ="TRUE"
    
    if(players[i,1] == "2011")
    {
      TeamInfo= Team2011.Info(players[i,1],as.character(players[i,2]),players[i,3])  
      TeamInfo = TeamInfo[-1,]
    }
    else if(players[i,1] == "2014")
    {
      TeamInfo= Team2014.Info(players[i,1],as.character(players[i,2]),players[i,3])  
    }
    else
    {
      TeamInfo = Team.Info(players[i,1],as.character(players[i,2]),players[i,3],players[i,4] ,players[i,5])              
      TeamInfo = TeamInfo[-1,]
    }
  
      
    
    if(first == "TRUE")
    {
      AllTeamInfo = TeamInfo
      first = "FALSE" 
    }else
    {
      AllTeamInfo = rbind(AllTeamInfo ,TeamInfo)
    }
  }
  i = i+1
}


##
##Append the team information to the excel file.
##
library(XLConnect)
# mtcars xlsx file from demoFiles subfolder of package XLConnect
wb <- loadWorkbook("PastTeamData.xls",create=FALSE)
appendWorksheet(wb, AllTeamInfo, sheet = "IPL Team Data")
saveWorkbook(wb,"PastTeamData.xls")




##
##Read all the player related data from PastTeamData.xls and update the info for the 2011 players from other years
##

TeamSquad = read.xlsx("PastTeamData.xls",1,stringsAsFactors=FALSE)

TeamSquad2011  = subset(TeamSquad,Year=="2011")


i=1
while(i < nrow(TeamSquad2011))
{
  
  TeamSquad2011[i,"PlayingRole"] = unique(as.character(TeamSquad[TeamSquad$Name==TeamSquad2011[i,"Name"],"PlayingRole"]))[1]
  TeamSquad2011[i,"Role"] = unique(as.character(TeamSquad[TeamSquad$Name==TeamSquad2011[i,"Name"],"Role"]))[1]
  
  TeamSquad2011[i,"BattingStyle"] = unique(as.character(TeamSquad[TeamSquad$Name==TeamSquad2011[i,"Name"],"BattingStyle"]))[1]
  TeamSquad2011[i,"BowlingStyle"] = unique(as.character(TeamSquad[TeamSquad$Name==TeamSquad2011[i,"Name"],"BowlingStyle"]))[1]
  
  
i=i+1
}

nrow(TeamSquad)

TeamSquad = TeamSquad[!TeamSquad$Year == "2011",]

TeamSquad = rbind(TeamSquad,TeamSquad2011)

write.xlsx2(TeamSquad,"PastTeamData.xls",sheetName="IPL Team Data")


##
## Clear the memory

TeamSquad=""
TeamSquad2011=""
AllTeamInfo=""
TeamInfo=""
PlayerDF=""
