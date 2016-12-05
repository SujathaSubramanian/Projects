###Base Model + countries KM and KM2 added as 20 columns(features)

library(data.table)
library(stringr)
source("Functions.R")
library(lubridate)
library(caret)
library(car)
library(xgboost)

##
##Loading train and test data. Combine train and test for all data conversion
##Save target variable separately and remove from train.
##
train <- read.csv('train_users_2.csv')
trainCSV = train
test <- read.csv('test_users.csv')

Target <- trainCSV$country_destination
train <- train[-grep('country_destination', colnames(train))]

train$TrainInd <- 1
test$TrainInd <- 2



data <- rbind(train,test)

colnames(data)[1] <- "user_id"

##
##Removing date_first_booking as it has direct co-relation with NDG target 
##variable
##

data = data[-grep('date_first_booking', colnames(train))]

##
##Basic handling of NAs in data, by initializing -1
##
data[is.na(data)] = -1



#~~~~~ DATE ACCOUNT CREATED SPLIT INTO YEAR, MONTH AND DAY FEATURES
dac = as.data.frame(str_split_fixed(data$date_account_created, '-', 3))
data['dac_year'] = dac[,1]
data['dac_month'] = dac[,2]
data['dac_day'] = dac[,3]
#data['dac_wday'] = wday(data$date_account_created)
data = data[,-c(which(colnames(data) %in% c('date_account_created')))]


#~~~~~ TIMESTAMP FIRST ACTIVE SPLIT INTO YEAR, MONTH AND DAY FEATURES
data[,'tfa_year'] = substring(as.character(data[,'timestamp_first_active']), 1, 4)
data['tfa_month'] = substring(as.character(data['timestamp_first_active']), 5, 6)
data['tfa_day'] = substring(as.character(data['timestamp_first_active']), 7, 8)
data = data[,-c(which(colnames(data) %in% c('timestamp_first_active')))]

rm(dac)

##
##Fix missing and incorrect data in age ####
##


data[,"age"][!(data$age>=14 & data$age <= 100)] = -1

#table(is.na(data$age))
#table(data$age)
#TODO:Create bins for age
#bins = apply(data[1:1000,"age"],1,generateBins_Age)
#discretize(data[1:1000,"age"],numBins=10,r=range(18,100))

##
##one-hot-encoding of all categorical features
##
ohe_feats = c('gender', 'signup_method','signup_flow', 'language','affiliate_channel','affiliate_provider','first_affiliate_tracked', 'signup_app', 'first_device_type','first_browser')
dummies <- dummyVars(~ gender + signup_method + signup_flow + language +  affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser , data = data)
df_all_ohe <- as.data.frame(predict(dummies, newdata = data))
data <- cbind(data[,-c(which(colnames(data) %in% ohe_feats))],df_all_ohe)
rm(df_all_ohe)

##
##This data is the basic data without a lot of feature engineering
##
write.csv(file="temp/baseData.csv",data,row.names = FALSE)

data = read.csv("temp/baseData.csv")


#
#Adding country features Km+Km2 from countries table to the Base Model
#

countries = read.csv("countries.csv")
km=data.frame(t(countries[,'distance_km']))
names(km) = paste("km_",as.character(countries$country_destination),sep="")
km=km[,-10]
km2=data.frame(t(countries[,'destination_km2']))
names(km2) = paste("km2_",as.character(countries$country_destination),sep="")
km = cbind(km,km2)


data= cbind(data,km)

##
##Adding sessions data

# 1) groupby = user_id, action 
#    summarize = count of records and sum of secsElapsed
#    
# 2) groupby = user_id, actionType 
#    summarize = count of records and sum of secsElapsed
#    
# 3) groupby = user_id, actionDetail 
#    summarize = count of records and sum of secsElapsed
# 
# Append with the base features   
##

##
##Basic handling of NAs in data, by initializing -1
##

actionFullWide<-read.csv(paste("temp/actionFullWide.csv"))
actionFullWide[is.na(actionFullWide)] = -1

data = merge(x=data,y=actionFullWide, 
             by.x = "user_id",by.y="user_id",all.x=TRUE) 

data[is.na(data)] = -1

rm(actionFullData,actionFullWide,by_user_id_actionFull,ithsession,temp)
rm(sessions)
##
##Splitting data back to train and test
##

train <- subset(data,data$TrainInd == 1)
test <-  subset(data,data$TrainInd == 2)

train = train[,-which(colnames(train)=="TrainInd")]
test = test[,-which(colnames(test)=="TrainInd")]


y <- recode(Target,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 
            'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 
            'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11",as.factor.result=FALSE)




##
##Setting parameters for XGBoost
##


xgb <- xgboost(data.matrix(train[,-1]), 
               label = y, 
               eta = 0.3,
               max_depth = 6, 
               nrounds=100, 
               subsample = 0.9,
               colsample_bytree = 0.7,
               #seed = 0,
               eval_metric = ndcg5,
               objective = "multi:softprob",
               num_class = 12,
               nthread = 8,
               learning_rate=0.3, 
               n_estimators=30,
               gamma=1)



# predict values in test set
y_pred <- predict(xgb, data.matrix(test[,-1]))
#y_pred



# extract the 5 classes with highest probabilities
predictions <- as.data.frame(matrix(y_pred, nrow=12))
rownames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
predictions_top5 <- as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))
# create submission 
idx = test$user_id
id_mtx <-  matrix(idx, 1)[rep(1,5), ]
ids <- c(id_mtx)
submission <- NULL
submission$id <- ids
submission$country <- predictions_top5

# generate submission file
submission <- as.data.frame(submission)
write.csv(submission, "submission/20160121_submission_10.csv", quote=FALSE, row.names = FALSE)





