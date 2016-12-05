##
#Compute a perentage of the population in a particluar age bucket for each country
##
generatePop_Ratio<-function(x)
{
  if(!exists("totalPop"))
    totalPop = ddply(agegender,"country_destination",summarise,total_pop = sum(population_in_thousands)) 
  
  binTotal = sum(x$population_in_thousands)
  ratio = (binTotal/totalPop[totalPop$country_destination == unique(x$country_destination),"total_pop"])*100
}

##
#Compute a perentage of the population in a particluar gender for each country
##
generateGender_Ratio<-function(x)
{
  if(!exists("totalPop"))
    totalPop = ddply(agegender,"country_destination",summarise,total_pop = sum(population_in_thousands)) 
  
  binTotal = sum(x$population_in_thousands)
  ratio = (binTotal/totalPop[totalPop$country_destination == x$country_destination,"total_pop"])*100
}

##
#For each age,return the pop ratio,for all the countries for the given age
##

findPopRatiofromAgeBucket <- function(x)
{
  #print(x$age)
  checkData[checkData$agll <=x$age & checkData$agul >= x$age,"popRatioAge"]
}

##
#For each age,return the pop ratio,for all the countries for the given age
##
findPopRatiofromGenderBucket <- function(x)
{
  checkData[checkData$gender == tolower(x$gender),"popRatioGender"]
}

sessionActionCount <- function(x)
{
   ddply(sessions[sessions$user_id == x$user_id,],'Actions',count)
}



ndcg5 <- function(preds, dtrain) {
  
  labels <- getinfo(dtrain,"label")
  num.class = 12
  pred <- matrix(preds, nrow = num.class)
  top <- t(apply(pred, 2, function(y) order(y)[num.class:(num.class-4)]-1))
  
  x <- ifelse(top==labels,1,0)
  dcg <- function(y) sum((2^y - 1)/log(2:(length(y)+1), base = 2))
  ndcg <- mean(apply(x,1,dcg))
  return(list(metric = "ndcg5", value = ndcg))
}



