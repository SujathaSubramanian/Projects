#Getting high level information of players

require(xlsx)

TeamNames = c("sunrisers-hyderabad","mumbai-indians","rajasthan-royals","kolkata-knight-riders",
             "kings-xi-punjab","chennai-super-kings","royal-challengers-bangalore",
             "delhi-daredevils")

Team.Info = function(teamname)
{
  fileurl = paste0("http://www.t20iplindia.com/",teamname)
  fileurl
  doc <- htmlParse(fileurl)
  doc
  tableNodes = getNodeSet(doc, "//table")
  Team.Info <-  readHTMLTable(tableNodes[[1]],
                              which=1,
                              stringsAsFactors=F)
  
  Roles = c("Batsman","Wicket-Keeper","All-Rounder","Bowler")
  
  subset(Team.Info,Team.Info$V2 %in% Roles)
  
}

#TeamNames = read.xlsx("Team.xlsx", sheetIndex = 2)
#TeamNames = TeamNames[1]
#i=1
#TeamNames[1]
TeamNames
while (i<= length(TeamNames))
{
  TeamInfo.table = Team.Info(TeamNames[i])
  names( TeamInfo.table) <- c("Players","TYPE","PRICE PAID (In Rs)")
  TeamInfo.table$TeamName = TeamNames[i]
  
  TeamInfo.table  = TeamInfo.table[sapply(TeamInfo.table, function(x) !any(is.na(x)))] 
  
  
  if (i ==1)
  {
      OverallTeamInfo = TeamInfo.table
  }
  else
  {
    OverallTeamInfo = rbind(OverallTeamInfo ,TeamInfo.table)
  }
  i=i+1
}



write.xlsx(OverallTeamInfo,"TeamData.xls",sheetName="2015 IPL Team Data")
