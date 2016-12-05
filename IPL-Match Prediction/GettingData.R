library(XML)
##Scrubbing URL file
require(xlsx)
players = read.xlsx("Team.xlsx", sheetName = "Team")
players = players[2]
i=1
while (i< nrow(players))
{
fileurl = paste0("http://www.cricmetric.com/playerstats.py?player=",players[i,1])
fileurl = paste0(fileurl,"&format=all&role=all")
fileurl
doc <- htmlParse(fileurl)
playerinfo <- xpathSApply(doc, "//div[@class='entry_content']/b", xmlValue)
nodes = getNodeSet(doc,"//div[@class='entry_content']/b/text()")


xmlValue(nodes[[3]])
names(xmlChildren(nodes[[1]]))

playerinfo <- xpathSApply(doc, "//div[@class='entry_content']/b", xmlValue)
xmlChildren(playerinfo)
playerinfo
playerinfo$children
playerinfo[[1]]
xmlValue
nodes

tableNodes = getNodeSet(doc, "table")
Test.table <-  readHTMLTable(tableNodes[[1]],
                            header=T, 
                            which=1,
                            stringsAsFactors=F)


}



??XML


playerinfo <- xpathSApply(doc, "//div[@class='entry_content']", xmlValue)
playerinfo
