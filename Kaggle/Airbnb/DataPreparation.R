library(data.table)
library(stringr)
source("Functions.R")
library(lubridate)
library(caret)

train <- read.csv('train_users_2.csv')
test <- read.csv('test_users.csv')

Target <- train$country_destination
train <- train[-grep('country_destination', colnames(train))]

#str(train)

train$TrainInd <- 1
test$TrainInd <- 2

data <- rbind(train,test)

rm(train,test)
colnames(data)[1] <- "user_id"


#~~~~~ DATE ACCOUNT CREATED SPLIT INTO YEAR, MONTH AND DAY FEATURES
dac = as.data.frame(str_split_fixed(data$date_account_created, '-', 3))
data['dac_year'] = dac[,1]
data['dac_month'] = dac[,2]
data['dac_day'] = dac[,3]
data['dac_wday'] = wday(data$date_account_created)
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
data['dfb_wday'] = wday(data$date_first_booking)
data = data[,-c(which(colnames(data) %in% c('date_first_booking')))]

rm(dac)

##Fix missing and incorrect data in age ####
#table(is.na(data$age))
#table(!(data$age>=18 & data$age <= 100))

medianAge = round(median(data[(data$age>=18 & data$age <= 100),"age"],na.rm=T))
meanAge = round(mean(data[(data$age>=18 & data$age <= 100),"age"],na.rm=T))

data[,"age"][is.na(data[,"age"])] <- meanAge
data[,"age"][!(data$age>=18 & data$age <= 100)] = meanAge

#table(is.na(data$age))
#table(data$age)
#TODO:Create bins for age
#bins = apply(data[1:1000,"age"],1,generateBins_Age)
#discretize(data[1:1000,"age"],numBins=10,r=range(18,100))

####Converting all data to factors
cat_features = c("signup_flow","tfa_year","tfa_month","tfa_day")
data[,cat_features] <- lapply(data[,cat_features] , factor)


####
###Bring in the pop ratio from the age_gender data , depending on the age of each record
####
agegender <- read.csv("age_gender_bkts.csv")
#age gender lower limit & #age gender upper limit
agegender$age_bucket = as.character(agegender$age_bucket)
agegender[agegender$age_bucket == "100+","age_bucket"] = "100"
agll = as.data.frame(str_split_fixed(agegender$age_bucket, '-', 2))
agegender['agll'] = as.character(agll[,1])
agegender['agul'] = as.character(agll[,2])
agegender[agegender$agll == "100","agul"] = "104"
agegender$agll = as.numeric(agegender$agll)
agegender$agul = as.numeric(agegender$agul)
str(agegender)

detach(package:dplyr)
library(plyr)
checkData = ddply(agegender,c('agll','agul','country_destination'),
                  function(x) c(popRatioAge=generatePop_Ratio(x)))


popRatioData = ddply(data,c('user_id'),
                     function(x) c(popRatioAge=findPopRatiofromAgeBucket(x)))


data = merge(data,popRatioData, by = c("user_id","user_id")) 

table(names(data) %in% "TrainInd")
####
###Bring in the pop ratio from the age_gender data , depending on the gender of each record
####
data$gender = as.character(data$gender)
str(agegender)
checkData = ddply(agegender,c('gender','country_destination'),
                  function(x) c(popRatioGender=generatePop_Ratio(x)))

checkData$gender = as.character(checkData$gender)
popRatioData = ddply(data[data$gender %in% c("FEMALE","MALE"),],c('user_id'),
                     function(x) c(popRatioGender=findPopRatiofromGenderBucket(x)))


data = merge(data,popRatioData, by = c("user_id","user_id"),all.x=TRUE) 


# one-hot-encoding features
ohe_feats = c('gender', 'signup_method','signup_flow', 'language','affiliate_channel','affiliate_provider','first_affiliate_tracked', 'signup_app', 'first_device_type','first_browser')
dummies <- dummyVars(~ gender + signup_method + signup_flow + language +  affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser , data = data)
df_all_ohe <- as.data.frame(predict(dummies, newdata = data))
data <- cbind(data[,-c(which(colnames(data) %in% ohe_feats))],df_all_ohe)
rm(df_all_ohe)


####
###Bring in the country distance and size from countries data set for each user
####

countries = read.csv("countries.csv")


km=data.frame(t(countries[,'distance_km']))
names(km) = paste("km_",as.character(countries$country_destination),sep="")
km=km[,-10]
km2=data.frame(t(countries[,'destination_km2']))
names(km2) = paste("km2_",as.character(countries$country_destination),sep="")
km = cbind(km,km2)

data= cbind(data,km)
table(names(data) %in% "TrainInd")

write.csv(file="temp/data.csv",data,row.names = FALSE)

rm(popRatioData,agll,agegender,checkData,countries,km,km2)
rm(data1)
####
###
####
sessions <- read.csv("sessions.csv")
##remove all records with no valid user_id
sessions$user_id = as.character(sessions$user_id)
sessions = sessions[-(which(nchar(sessions$user_id) == 0)),]

###############reading in splitwise manner############################################
uniqueUserIds<-unique(sessions$user_id)
splits<-split(uniqueUserIds,ceiling(1:length(uniqueUserIds)/10000))
str(splits)

#Load the CSV files and add to one data frame

##Get the count of time elapsed by action
detach(package:plyr)
library(dplyr)

print("processing action from sessions")
for (i in 1:length(splits))
{
  
  ithsession = sessions[which(sessions$user_id %in% splits[[i]]),]
  by_user_id_action <- group_by(ithsession, user_id,action)
  actionTimeData <- summarise(by_user_id_action, count = length(user_id),
                              sum = sum(secs_elapsed)
  )
  
  write.csv(file=paste("temp/actionTimeData_",i,".csv"),actionTimeData,row.names = FALSE)
  print(paste(round(i*100/length(splits))," % done"))
}

rm(actionTimeData,ithsession,by_user_id_action)

print("processing action type from sessions")


for (i in 1:length(splits))
{
  
  ithsession = sessions[which(sessions$user_id %in% splits[[i]]),]
  by_user_id_actionType <- group_by(ithsession, user_id,action_type)
  actionTypeData <- summarise(by_user_id_actionType, count = length(user_id),
                              sum = sum(secs_elapsed)
  )
  
  write.csv(file=paste("temp/actionTypeData_",i,".csv"),actionTypeData,row.names = FALSE)
  print(paste(round(i*100/length(splits))," % done"))
}

rm(actionTypeData,ithsession,by_user_id_actionType)



print("processing action detail from sessions")


for (i in 1:length(splits))
{
  
  ithsession = sessions[which(sessions$user_id %in% splits[[i]]),]
  by_user_id_actionDetail <- group_by(ithsession, user_id,action_detail)
  actionDetailData <- summarise(by_user_id_actionDetail, count = length(user_id),
                                sum = sum(secs_elapsed)
  )
  
  write.csv(file=paste("temp/actionDetailData_",i,".csv"),actionDetailData,row.names = FALSE)
  print(paste(round(i*100/length(splits))," % done"))
}

rm(actionDetailData,ithsession,by_user_id_actionDetail,sessions)


print("LOading action related session data")

##Loading it back
for (i in (1:length(splits)))
{
  temp<-read.csv(paste("temp/actionTimeData_",i,".csv"))
  if (i==1)
  {
    actionTimeData<-temp
    
  }
  else
  {
    print (i)
    colnames(temp)<-colnames(actionTimeData)
    actionTimeData<-rbind(actionTimeData,temp)
  }
  
}


cols = c('user_id','action','count')
actionCountWide = reshape(actionTimeData[,cols],v.names="count",
                          idvar = "user_id",
                          timevar="action",direction="wide")

cols = c('user_id','action','sum')
actionMeanWide = reshape(actionTimeData[,cols],v.names="sum",
                         idvar = "user_id",
                         timevar="action",direction="wide")


actionMeanWide = actionMeanWide[,-1]

actionWide = cbind(actionCountWide,actionMeanWide[,-1])
rm(actionTimeData,actionCountWide,actionMeanWide)

print("LOading action type related session data")
for (i in (1:length(splits)))
{
  temp<-read.csv(paste("temp/actionTypeData_",i,".csv"))
  if (i==1)
  {
    actionTypeData<-temp
    
  }
  else
  {
    print (i)
    colnames(temp)<-colnames(actionTypeData)
    actionTypeData<-rbind(actionTypeData,temp)
  }
  
}


cols = c('user_id','action_type','count')
actionTypeCountWide = reshape(actionTypeData[,cols],v.names="count",
                              idvar = "user_id",
                              timevar="action_type",direction="wide")

cols = c('user_id','action_type','sum')
actionTypeMeanWide = reshape(actionTypeData[,cols],v.names="sum",
                             idvar = "user_id",
                             timevar="action_type",direction="wide")



actionTypeWide = cbind(actionTypeCountWide[,-1],actionTypeMeanWide[,-1])


rm(actionTypeData,actionTypeCountWide,actionTypeMeanWide)

print("LOading action detail related session data")
for (i in (1:length(splits)))
{
  temp<-read.csv(paste("temp/actionDetailData_",i,".csv"))
  if (i==1)
  {
    actionDetailData<-temp
    
  }
  else
  {
    print (i)
    colnames(temp)<-colnames(actionDetailData)
    actionDetailData<-rbind(actionDetailData,temp)
  }
  
}


cols = c('user_id','action_detail','count')
actionDetailCountWide = reshape(actionDetailData[,cols],v.names="count",
                                idvar = "user_id",
                                timevar="action_detail",direction="wide")

cols = c('user_id','action_detail','sum')
actionDetailMeanWide = reshape(actionDetailData[,cols],v.names="sum",
                               idvar = "user_id",
                               timevar="action_detail",direction="wide")



actionDetailWide = cbind(actionDetailCountWide[,-1],actionDetailMeanWide[,-1])
rm(actionDetailData,actionDetailCountWide,actionDetailMeanWide)


actionWide = cbind(actionWide,actionTypeWide,actionDetailWide)
rm(actionTypeWide,actionDetailWide)




write.csv(file="temp/actionWide.csv",actionWide,row.names = FALSE)

data_final = merge(x=data,y=actionWide, by.x = "user_id",by.y="user_id",all.x=TRUE) 
rm(actionWide,data,temp)
rm(sessionid,train,test)
rm(checkData)
gc()

#train <- subset(data_final,data_final$TrainInd == 1)
#test <-  subset(data_final,data_final$TrainInd == 2)




table(names(data) %in% "TrainInd")

