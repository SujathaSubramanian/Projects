require(readr)
require(plyr)
require(sqldf)
require(stringr)
require(caret)
require(car)
library(Matrix)
library(data.table)
library(plyr)
library(entropy)
library(stringr)
require(bit64)



train <- read.csv('train_users_2.csv')
test <- read.csv('test_users.csv')


Target <- train$country_destination
train <- train[-grep('country_destination', colnames(train))]

train$TrainInd <- 1
test$TrainInd <- 2

data <- rbind(train,test)
colnames(data)[1] <- "user_id"

sessions <- read.csv('sessions.csv')

#~~~~~ REMOVE ID'S UNDER 18 AND OVER 90 
#data <- subset(data,data$age >18)
#data <- subset(train,data$age < 100)

#~~~~~ IMPUTE AGE
data[,6][is.na(data[,6])] <- median(data[,6], na.rm = T)

#~~~~~ DATE ACCOUNT CREATE SPLIT INTO YEAR, MONTH AND DAY FEATURES
dac = as.data.frame(str_split_fixed(data$date_account_created, '-', 3))
data['dac_year'] = dac[,1]
data['dac_month'] = dac[,2]
data['dac_day'] = dac[,3]
data = data[,-c(which(colnames(data) %in% c('date_account_created')))]

#~~~~~ TIMESTAMP FIRST ACTIVE SPLIT INTO YEAR, MONTH AND DAY FEATURES
data[,'tfa_year'] = substring(as.character(data[,'timestamp_first_active']), 1, 4)
data['tfa_month'] = substring(as.character(data['timestamp_first_active']), 5, 6)
data['tfa_day'] = substring(as.character(data['timestamp_first_active']), 7, 8)
data = data[,-c(which(colnames(data) %in% c('timestamp_first_active')))]

#~~~~~ DATE ACCOUNT CREATE SPLIT INTO YEAR, MONTH AND DAY FEATURES
dac = as.data.frame(str_split_fixed(data$date_first_booking, '-', 3))
data['dfb_year'] = dac[,1]
data['dfb_month'] = dac[,2]
data['dfb_day'] = dac[,3]
data = data[,-c(which(colnames(data) %in% c('date_first_booking')))]


feature.names <- names(data)[1:ncol(data)]

for (f in feature.names) {
  if (class(data[[f]])=="character") {
    levels <- unique(c(data[[f]]))
    data[[f]] <- as.integer(factor(data[[f]], levels=levels))
  }
}

rm(list=c('f','feature.names','levels','train','test','dac'))
gc()

#***********************************************************************************
#********************************** SESSIONS DATA **********************************
#***********************************************************************************

#~~~~~ UNIQUE VALUES FOR EACH COLUMN IN SESSIONS DATA
Users <- data.frame('user_id'=unique(sessions$user_id))
Actions <- unique(sessions$action)
ActionTypes <- unique(sessions$action_type)
ActionDetails <- unique(sessions$action_detail)
Devices <- unique(sessions$device_type)

Actions
Actions[[1]]
#~~~~~ FIX NUMERIC NAMES IN ACTIONS 
Actions[[55]] <- "Fifteen"
Actions[57] <- "Twelve"
Actions[73] <- "Ten"
Actions[159] <- "Eleven"
Actions[222] <- "southern_europe"
Actions[280] <- "social_media"
Actions[281] <- "united_states"

#~~~~~ AGGREGATE SESSION DATA - COUNT OF ACTIONS

for (i in 1:length(Actions)){
  i=1
  name <- Actions[i]
  temp1 <- subset(sessions,sessions$action == name)
  query <- paste("select user_id,count(user_id) as ",name,sep="")
  query<- paste(query,"_AFreq from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}

#~~~~~ AGGREGATE SESSION DATA - AVERAGE SECS OF ACTIONS

for (i in 1:length(Actions)){
  name <- Actions[i]
  temp1 <- subset(sessions,sessions$action == name)
  query <- paste("select user_id,avg(secs_elapsed) as ",name,sep="")
  query<- paste(query,"_AAVG from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}

#~~~~~ AGGREGATE SESSION DATA - MIN SECS OF ACTIONS

for (i in 1:length(Actions)){
  name <- Actions[i]
  temp1 <- subset(sessions,sessions$action == name)
  query <- paste("select user_id,min(secs_elapsed) as ",name,sep="")
  query<- paste(query,"_AMIN from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}

#~~~~~ AGGREGATE SESSION DATA - MAX SECS OF ACTIONS

for (i in 1:length(Actions)){
  name <- Actions[i]
  temp1 <- subset(sessions,sessions$action == name)
  query <- paste("select user_id,max(secs_elapsed) as ",name,sep="")
  query<- paste(query,"_AMAX from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}

#~~~~~ FIX NUMERIC NAMES IN LIST
ActionTypes[7] <- "unknown"

#~~~~~ AGGREGATE SESSION DATA - COUNT OF ACTION TYPES

for (i in 1:length(ActionTypes)){
  name <- ActionTypes[i]
  temp1 <- subset(sessions,sessions$action_type == name)
  query <- paste("select user_id,count(user_id) as ",name,sep="")
  query<- paste(query,"_ATFreq from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}

#~~~~~ AGGREGATE SESSION DATA - AVERAGE SECS OF ACTION TYPES

for (i in 1:length(ActionTypes)){
  name <- ActionTypes[i]
  temp1 <- subset(sessions,sessions$action_type == name)
  query <- paste("select user_id,avg(secs_elapsed) as ",name,sep="")
  query<- paste(query,"_ATAVG from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}

#~~~~~ AGGREGATE SESSION DATA - MIN SECS OF ACTION TYPES

for (i in 1:length(ActionTypes)){
  name <- ActionTypes[i]
  temp1 <- subset(sessions,sessions$action_type == name)
  query <- paste("select user_id,min(secs_elapsed) as ",name,sep="")
  query<- paste(query,"_ATMIN from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}

#~~~~~ AGGREGATE SESSION DATA - MAX SECS OF ACTION TYPES

for (i in 1:length(ActionTypes)){
  name <- ActionTypes[i]
  temp1 <- subset(sessions,sessions$action_type == name)
  query <- paste("select user_id,max(secs_elapsed) as ",name,sep="")
  query<- paste(query,"_ATMAX from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}


#~~~~~ FIX NUMERIC NAMES IN LIST
ActionDetails[10] <- "unknown"

#~~~~~ AGGREGATE SESSION DATA - COUNT OF ACTION DETAILS

for (i in 1:length(ActionDetails)){
  name <- ActionDetails[i]
  temp1 <- subset(sessions,sessions$action_detail == name)
  query <- paste("select user_id,count(user_id) as ",name,sep="")
  query<- paste(query,"_ADFreq from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}

#~~~~~ AGGREGATE SESSION DATA - AVERAGE OF ACTION DETAILS

for (i in 1:length(ActionDetails)){
  name <- ActionDetails[i]
  temp1 <- subset(sessions,sessions$action_detail == name)
  query <- paste("select user_id,avg(secs_elapsed) as ",name,sep="")
  query<- paste(query,"_ADAVG from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}

#~~~~~ AGGREGATE SESSION DATA - MIN OF ACTION DETAILS

for (i in 1:length(ActionDetails)){
  name <- ActionDetails[i]
  temp1 <- subset(sessions,sessions$action_detail == name)
  query <- paste("select user_id,min(secs_elapsed) as ",name,sep="")
  query<- paste(query,"_ADMIN from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}

#~~~~~ AGGREGATE SESSION DATA - MAX OF ACTION DETAILS

for (i in 1:length(ActionDetails)){
  name <- ActionDetails[i]
  temp1 <- subset(sessions,sessions$action_detail == name)
  query <- paste("select user_id,max(secs_elapsed) as ",name,sep="")
  query<- paste(query,"_ADMAX from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}

#~~~~~ FIX NUMERIC NAMES IN  LIST
Devices[1] <- 'Windows'
Devices[2] <- "unknown"
Devices[3] <- 'Mac'
Devices[4] <- 'Android_phone'
Devices[6] <- 'iPad'
Devices[7] <- 'Android_App'
Devices[8] <- 'Linux'
Devices[13] <- 'Windows_phone'
Devices[14] <- 'Opera_phone'

#~~~~~ AGGREGATE SESSION DATA - COUNT OF DEVICES

for (i in 1:length(Devices)){
  name <- Devices[i]
  temp1 <- subset(sessions,sessions$device_type == name)
  query <- paste("select user_id,count(user_id) as ",name,sep="")
  query<- paste(query,"_DFreq from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}


#~~~~~ AGGREGATE SESSION DATA - AVG OF DEVICES

for (i in 1:length(Devices)){
  name <- Devices[i]
  temp1 <- subset(sessions,sessions$device_type == name)
  query <- paste("select user_id,avg(secs_elapsed) as ",name,sep="")
  query<- paste(query,"_DAVG from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}

#~~~~~ AGGREGATE SESSION DATA - MIN OF DEVICES

for (i in 1:length(Devices)){
  name <- Devices[i]
  temp1 <- subset(sessions,sessions$device_type == name)
  query <- paste("select user_id,min(secs_elapsed) as ",name,sep="")
  query<- paste(query,"_DMIN from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}

#~~~~~ AGGREGATE SESSION DATA - MAX OF DEVICES

for (i in 1:length(Devices)){
  name <- Devices[i]
  temp1 <- subset(sessions,sessions$device_type == name)
  query <- paste("select user_id,max(secs_elapsed) as ",name,sep="")
  query<- paste(query,"_DMAX from temp1 group by user_id",sep="")
  temp2<-sqldf(query)
  Users <- merge(x=Users,y=temp2,by="user_id",all.x = T)
  print(i)
  gc()
}

rm(list=c('temp1','temp2','sessions','train','test','ActionDetails','Actions','ActionTypes','Devices','i','name','query'))
gc()
#~~~~~ COMBINE SESSION DATA WITH TRAIN AND TEST DATA
data <- merge(x=data,y=Users,by="user_id",all.x = T)
rm(list=c('Users'))
gc()

train <- subset(data,data$TrainInd == 1)
test <-  subset(data,data$TrainInd == 2)
