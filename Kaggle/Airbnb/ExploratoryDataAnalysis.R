# Author : Sujatha Subramanian
# https://www.kaggle.com/c/AirBnb/
# ########################################################################
library(Matrix)
library(data.table)
library(plyr)
library(entropy)
library(stringr)
require(bit64)

#Loading the train and test data
train<-fread("train_users_2.csv")
test<-fread("test_users.csv")
agegender <- fread("age_gender_bkts.csv")
sessions <- fread("sessions.csv")
countries <- fread("countries.csv")

#gives a very brief description of the data
str(train)
#gives the name of each variables
names(train) 
#gives some very basic summary statistics for each variable
summary(train) 
#shows the first few rows
head(train) 
#shows the last few rows.
tail(train) 

nrow(train)


table(train[train$age>100,country_destination])

####################################################################
##########Data in the agegender Demographics Data Frame ############
####################################################################
str(agegender)
agegender[agegender$country_destination == "AU"]
agegender[agegender$country_destination == "US"]
table(agegender$country_destination) ##42 records per country in agegender data
table(agegender[agegender$country_destination == "US",gender]) ## 21 records per gender
##21 age wise buckets per gender per country
table(agegender[agegender$country_destination == "US" & agegender$gender == "female",age_bucket])
sort(unique(agegender$age_bucket)) # 21 age buckets

table(agegender$year) # ONly 2015
table(agegender$country_destination) # 10 countries (AU CA DE ES FR GB IT NL PT US)


#Summary
#All the records are for the year 2015. Each record represents the population of each of the 21 
#age bucket per each gender for each of the 10 countries


#NOTE:
#Population of australia in 2015 in all age groups among both the gender 
sum(agegender[agegender$country_destination == "AU",population_in_thousands])
sum(agegender[agegender$country_destination == "US",population_in_thousands])

##This population information in line with the demographics info of the united states as 
##mentioned in wikipedia "https://en.wikipedia.org/wiki/Demography_of_the_United_States"


########################################################
##########Data in the train/test Data Frame ############
########################################################

str(train)
#####ID related data - #########
length(unique(train$id)) # - One record per id
table(is.na(train$id))
table(is.na(test$id))

#####Date related data- #########
table(train$date_account_created )
table(test$date_account_created )
table(is.na(train$date_account_created ))
#In Training set -  Date is ranging from - 2010-01-01 to 2014-06-30 
#In Testing set -  Date ranging from 2014-07-01 to 2014-09-30 

summary(train$date_account_created)

#####Timestamp first active- #########
table(is.na(train$timestamp_first_active ))
##this is a integer 64
##TODO: Need to find out how to handle timestamp 64 bit data


#####Gender related data - #########
table(train$gender)
table(test$gender)
table(is.na(train$gender))
#-unknown-    FEMALE      MALE     OTHER 
#95688     63041     54440       282
#TODO: How to handle -unknown in gender

#####Age related data - #########
summary(train$age)
#TODO : age has 87990 data as NULL.
#TODO : how to handle age mentioned as < 18 and > 100
table(train[train$age>100]$age)
table(train[train$age<18]$age)

table(test[test$age>100]$age)
table(test[test$age<18]$age)

table(is.na(train$age))
table(is.na(test$age))

table(train$age)

#####signup_method related data - #########
table(is.na(train$signup_method))
table(train$signup_method) ## basic , facebook or google
table(test$signup_method) ##basic,facebook,google,weibo
#TODO: Testing data has an extra category which is not in train ("weibo")

table(is.na(train$signup_flow))
table(train$signup_flow) 


#####language related data - categorical data#########
table(is.na(train$language))
table(is.na(test$language))

sort(table(train$language))
sort(table(test$language))
##ToDO - test data has one unknown

#####affiliate_channel - related data #########
table(is.na(train$affiliate_channel))
table(is.na(test$affiliate_channel))
sort(table(train$affiliate_channel))
sort(table(test$affiliate_channel))

#####affiliate_provider- related data #########
table(is.na(train$affiliate_provider))
table(is.na(test$affiliate_provider))
sort(table(train$affiliate_provider))
sort(table(test$affiliate_provider))

test[!test$affiliate_provider %in% train$affiliate_provider,affiliate_provider]

#####first_affiliate_tracked- related data #########
table(is.na(train$first_affiliate_tracked))
table(is.na(test$first_affiliate_tracked))
sort(table(train$first_affiliate_tracked))
sort(table(test$first_affiliate_tracked))

test[!test$first_affiliate_tracked %in% train$first_affiliate_tracked,first_affiliate_tracked]
##TODO:large percentage of records are untracked.Ex: In train it is 109232

#####signup_app- related data #########
table(is.na(train$signup_app))
table(is.na(test$signup_app))
sort(table(train$signup_app))
sort(table(test$signup_app))

test[!test$signup_app %in% train$signup_app,signup_app]

#####first_device_type- related data #########
table(is.na(train$first_device_type))
table(is.na(test$first_device_type))
sort(table(train$first_device_type))
sort(table(test$first_device_type))

test[!test$first_device_type %in% train$first_device_type,first_device_type]

#####first_browser- related data #########
table(is.na(train$first_browser))
table(is.na(test$first_browser))
sort(table(train$first_browser))
sort(table(test$first_browser))

test[!test$first_browser %in% train$first_browser,first_browser]

#####country_destination- related data #########
table(is.na(train$country_destination))
sort(table(train$country_destination))


####################################################################
##########Data in the countries Data Frame ########################
####################################################################

str(countries)

table(is.na(countries$country_destination)) ## Data for all the 10 countries
table(is.na(countries$lat_destination)) 
table(is.na(countries$lng_destination)) 
countries$lat_destination

table(is.na(countries$distance_km)) 
countries$distance_km ## It is distance from US to each of the countries.

table(is.na(countries$destination_km2)) 
countries$destination_km2
#TODO: What is this column ? why is value for US/CA/AU so much higher 

table(is.na(countries$destination_language)) 
countries$destination_language

table(is.na(countries$language_levenshtein_distance)) 
sort(countries$language_levenshtein_distance)
countries[order(countries$language_levenshtein_distance)]

unique(train$country_destination)

head(sessions)

userd1mm9tcy42 = sessions[sessions$user_id == "d1mm9tcy42"]
unique(userd1mm9tcy42$action_detail)

unique(sessions$action_detail)
unique(sessions$action_type)
unique(sessions$action)

table(train$country_destination == "NDF")

sessions[sessions$action == "sandy"]
userd1mm9tcy42 = sessions[sessions$user_id == "ej8rmqaosu"]


sort(table(sessions$action))
sort(table(sessions$action_detail))
sort(table(sessions$action_type))




##Boxplot of target country average age
summary(train)
!is.na(train$age)
boxplot(train[!(is.na(train$age) | train$age >150),age]~
        train[!(is.na(train$age) | train$age >150),country_destination])

##Boxplot of target country and gender
boxplot(train[!is.na(train$gender),gender]~
          train[!is.na(train$gender),country_destination])


hist(train[!(is.na(train$age) | train$age >100),age])


          US              FR                GER       
Male      #count to US
Female


##Gender wise Count of trip to country_destination

x = ddply(train[train$country_destination != "NDF"],.(country_destination,gender),count)

wide <- reshape(x, v.names = "n", idvar = "gender",
                 timevar = "country_destination",direction = "wide")

barplot(as.matrix(wide[2:12])) # default



##age,gender,age*gender with country_destination
table(is.na(train$country_destination))



model = lm(age~country_destination,train[!is.na(train$age)])
summary(model)



###