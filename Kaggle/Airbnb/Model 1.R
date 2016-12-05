###Model 1
library(xgboost)
library(car)
source("Functions.R")

train = read.csv('train_users_2.csv')
labels = train['country_destination']

data<-read.csv(paste("temp/data.csv"))
actionWide<-read.csv(paste("temp/actionWide.csv"))


data_final = merge(x=data,y=actionWide, by.x = "user_id",by.y="user_id",all.x=TRUE) 

rm(actionWide,data)
gc()


train <- subset(data_final,data_final$TrainInd == 1)
test <-  subset(data_final,data_final$TrainInd == 2)


train = train[,-which(colnames(train)=="TrainInd")]
test = test[,-which(colnames(test)=="TrainInd")]

rm(data_final)
gc()

#names(train)
#train = train[,1:762]
#test = test[,1:762]

train[is.na(train)] <- -1
test[is.na(test)] <- -1


y <- recode(labels$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 
                                        'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 
                                        'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11",as.factor.result=FALSE)


xgb <- xgboost(data = data.matrix(train[,-1]), 
               label = y, 
               eta = 0.03,
               max_depth = 30, 
               nround=10, 
               subsample = 0.6,
               colsample_bytree = 0.6,
               #seed = 0,
               eval_metric = ndcg5,
               objective = "multi:softprob",
               num_class = 12,
               nthread = 8
)


# predict values in test set
y_pred <- predict(xgb, data.matrix(test[,-1]))
y_pred


# extract the 5 classes with highest probabilities
predictions <- as.data.frame(matrix(y_pred, nrow=12))
rownames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
predictions_top5 <- as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))
head(predictions)
# create submission 
idx = test$user_id
id_mtx <-  matrix(idx, 1)[rep(1,5), ]
ids <- c(id_mtx)
# 10,000 times faster in my local testing:
# system.time({idx = X_test$id; id_mtx <-  matrix(idx, 1)[rep(1,5), ]; ids <- c(id_mtx)})
#   user  system elapsed 
#  0.009   0.002   0.011 
# system.time({ids<-NULL; for (i in 1:NROW(X_test)){idx <- X_test$id[i]; ids <- append(ids, rep(idx,5))}})
#   user  system elapsed 
#108.056  54.932 166.038
#ids <- NULL
#for (i in 1:NROW(X_test)) {
#  idx <- X_test$id[i]
#  ids <- append(ids, rep(idx,5))
#}
submission <- NULL
submission$id <- ids
submission$country <- predictions_top5

head(submission)

# generate submission file
submission <- as.data.frame(submission)
write.csv(submission, "submission/20160121_submission_3.csv", quote=FALSE, row.names = FALSE)
##1. .0.85223


importance_matrix <- xgb.importance(model = xgb)
print(importance_matrix)
xgb.plot.importance(importance_matrix)

head(xgb.dump(xgb, with.stats = F))

