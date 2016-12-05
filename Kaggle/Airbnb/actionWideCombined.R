sessions = read.csv("sessions.csv")
#actionWide<-read.csv(paste("temp/actionWide.csv"))
head(sessions)

##remove all records with no valid user_id
sessions$user_id = as.character(sessions$user_id)
sessions = sessions[-(which(nchar(sessions$user_id) == 0)),]

sessions$actionFull = paste(sessions$action,sessions$action_type,sessions$action_detail,sep="_")
unique(sessions$actionFull)

uniqueUserIds<-unique(sessions$user_id)
splits<-split(uniqueUserIds,ceiling(1:length(uniqueUserIds)/10000))
str(splits)

#Load the CSV files and add to one data frame

##Get the count of time elapsed by action
library(dplyr)


print("processing actionFull from sessions")
for (i in 1:length(splits))
{
  
  ithsession = sessions[which(sessions$user_id %in% splits[[i]]),]
  by_user_id_actionFull <- group_by(ithsession, user_id,actionFull)
  actionFullData <- summarise(by_user_id_actionFull, count = length(user_id),
                              sum = sum(secs_elapsed)
  )
  write.csv(file=paste("temp/actionFullData_",i,".csv"),actionFullData,row.names = FALSE)
  print(paste(round(i*100/length(splits))," % done"))
}


##Loading it back
for (i in (1:length(splits)))
{
  temp<-read.csv(paste("temp/actionFullData_",i,".csv"))
  if (i==1)
  {
    actionFullData<-temp
    
  }
  else
  {
    print (i)
    colnames(temp)<-colnames(actionFullData)
    actionFullData<-rbind(actionFullData,temp)
  }
  
}


cols = c('user_id','actionFull','count')
actionFullCountWide = reshape(actionFullData[,cols],v.names="count",
                          idvar = "user_id",
                          timevar="actionFull",direction="wide")

cols = c('user_id','actionFull','sum')
actionFullSumWide = reshape(actionFullData[,cols],v.names="sum",
                         idvar = "user_id",
                         timevar="actionFull",direction="wide")


#actionFullSumWide = actionFullSumWide[,-1]

actionFullWide = cbind(actionFullCountWide,actionFullSumWide[,-1])



write.csv(file="temp/actionFullWide.csv",actionFullWide,row.names = FALSE)

rm(actionFullSumWide,actionFullCountWide)
