#library--------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
memory.limit(size=56000)
setwd("C:\\Users\\user\\Desktop\\rossmann-store-sales")
#data =====================================================================================================================================================
store = read.csv("store.csv")%>%as.data.frame()    #nrow : 1115
train = read.csv("train.csv")%>%as.data.frame()  #nrow: 1017209
test = read.csv("test.csv")%>%as.data.frame()    #nrow:41088
#data manipulation==========================================================================================================================================
##NA check--------------------------------------------------------------------------------------
#check (Promo2==0 ->NA)
View(select(store,Promo2,Promo2SinceWeek,Promo2SinceYear,PromoInterval)%>%filter(store$Promo2==0))
#check (Competiton->NA)
View(select(store,CompetitionDistance,CompetitionOpenSinceMonth,CompetitionOpenSinceYear)%>%filter(is.na(store$CompetitionOpenSinceMonth)==TRUE))
na_sum<-apply(store, 2, function(x) sum(is.na(x)))
barplot(na_sum[na_sum>0],main = "Na count")
store[is.na(store)] <- 0  #NA->0로 대체 
#Open --------------------------------------------------------------------------------------
train[is.na(train$Open),] ; #없음 
test[is.na(test$Open), ] #622번 
open_count<- test%>%group_by(DayOfWeek,Open)%>%summarise(n=n())
open_count<-data.frame(open_count[which(!is.na(open_count$Open)),])
open_count$Open<-as.factor(open_count$Open)
ggplot(data=open_count,aes(x=DayOfWeek,y=n,fill=Open))+geom_bar(stat="identity")
test$Open[is.na(test$Open)] <- 1  #NA->1로 대체 
##date format--------------------------------------------------------------------------------------
###date format change, order sort
train$Date=as.Date(train$Date) ;test$Date=as.Date(test$Date)
train <- train[order(train$Date),] ;row.names(train)<-NULL
test <- test[order(test$Date),] ;row.names(test)<-NULL
##data range
range(train$Date) #"2013-01-01" "2015-07-31"
range(test$Date) #"2015-08-01" "2015-09-17"
###date-> month, year, day (var change)
train$Year <- as.integer(format(train$Date, "%Y"))
train$Month <- as.integer(format(train$Date, "%m"))
train$Day <- as.integer(format(train$Date, "%d"))
test$Year <- as.integer(format(test$Date, "%Y"))
test$Month <- as.integer(format(test$Date, "%m"))
test$Day <- as.integer(format(test$Date, "%d"))
##plot--------------------------------------------------------------------------------------
ggplot(data.frame(train[which(train$Open==1),]),aes(x=Customers))+geom_histogram(binwidth = 10) 
ggplot(data.frame(train[which(train$Open==1),]),aes(x=Sales))+geom_histogram(binwidth = 10)
ggplot(data.frame(train[which(train$Open==1),]),aes(x=log(Customers+1),y=log(Sales+1)))+geom_point()+geom_smooth(method="lm")
ggplot(data.frame(train[which(train$Open==1),]),aes(x=log(Customers+1),y=log(Sales+1),col=factor(Promo)))+geom_point()+geom_smooth(method="lm")
ggplot(data.frame(train[which(train$Open==1),]),aes(x = Date, y = Sales, color = factor(Promo)))+geom_point()+geom_smooth(method="lm")
##merge--------------------------------------------------------------------------------------
train_store <- merge(train, store, by = "Store") ; test<-merge(test,store,by="Store")
ggplot(train_store%>%filter(Sales!=0),aes(x = Date, y = Sales, color = factor(StoreType))) + 
  geom_smooth(size = 2)
ggplot(train_store%>%filter(Customers != 0), aes(x = Date, y = Customers, color = factor(StoreType))) + 
  geom_smooth(size = 2)
ggplot(train_store%>%filter(Sales!=0), aes(x = Date, y = Sales, color = factor(Assortment))) + 
  geom_smooth(size = 2)
ggplot(train_store%>%filter(Customers != 0), aes(x = Date, y = Customers, color = factor(Assortment))) + 
  geom_smooth(size = 2)
##Competition months , Promo2 weeks--------------------------------------------------------------------------------------
train_store[,"Competitionmthdiff"]<-12*(train_store$Year-train_store$CompetitionOpenSinceYear)+
  train_store$Month-train_store$CompetitionOpenSinceMonth
train_store[,"Promo2wdiff"]<-12*4*(train_store$Year-train_store$Promo2SinceYear)+
  week(train_store$Date)-train_store$Promo2SinceWeek
test[,"Competitionmthdiff"]<-12*(test$Year-test$CompetitionOpenSinceYear)+
  test$Month-test$CompetitionOpenSinceMonth
test[,"Promo2wdiff"]<-12*4*(test$Year-test$Promo2SinceYear)+
  week(test$Date)-test$Promo2SinceWeek
#필요없는 변수들 삭제-------------------------------------------------------------------------------------- 
train_store<-train_store[,-c(3,5,16,17,19,20,21)]
test<-test[,-c(4,15,16,18,19,20)]
#write.csv(train_store,"train_store.csv")
#write.csv(test,"test2.csv")
#train_store<-read.csv("train_store.csv");test<-read.csv("test2.csv")
#train_store<-train_store[,-1];test<-test[,-1]
#modeling==========================================================================================================================================
##CV :sampling ------------- ---------------------------------------------------------------------------------------
set.seed(1004) ; 
train_store<-train_store%>%filter(Open==1)%>%filter(Sales>0) #Sales >0이고 Open=1
spl <- sample(1:2, nrow(train_store), replace = T, prob = c(0.7, 0.3))
mytrain <- train_store[(spl==1),] ; myvalid <- train_store[(spl==2),]
levels<-list()
factors<-c("StateHoliday","StoreType","Assortment")
for(i in 1:length(factors)){
  levels[[i]]<-unique(mytrain[,factors[i]])
  mytrain[,factors[i]]<-as.integer(factor(mytrain[,factors[i]],levels=levels[[i]])) 
  myvalid[,factors[i]]<-as.integer(factor(myvalid[,factors[i]],levels=levels[[i]]))
  test[,factors[i]]<-as.integer(factor(test[,factors[i]],levels=levels[[i]])) 
}
#write.csv(mytrain,"mytrain.csv");write.csv(myvalid,"myvalid.csv");
#mytrain<-read.csv("mytrain.csv");myvalid<-read.csv("myvalid.csv");
#mytrain<-mytrain[,-1];myvalid<-myvalid[,-1]
##modeling--------------------------------------------------------------------------------------
rmspe<-function(y, yhat){
  return (sqrt(mean((yhat/y-1)^2)))
}
test_open<-test%>%filter(Open==1) ; test_closed<-test%>%filter(Open==0)
#randomforest----------------------------------------------------------------------------------
library(randomForest)
rf<- randomForest(mytrain[,-c(1,3,4)],log(mytrain$Sales+1),
                  mtry=5,
                  ntree=50,
                  sampsize=100000,
                  do.trace=TRUE) #Sales, Open 제외 
pred.rf<- exp(predict(rf, newdata=myvalid[,-c(1,3,4)],type='response'))-1
err1<-rmspe(myvalid$Sales,pred.rf)
err1 #0.2631

pred.f.rf<-exp(predict(rf,test_open[,-c(1,2,4)],type='response'))-1
open.result<-data.frame(Id=test_open$Id,Sales=pred.f.rf);
rf.result<-merge(data.frame(test$Id),open.result,by="Id",all=TRUE) ; rf.result[is.na(rf.result)] <- 0 
write_csv(rf.result, "rf.csv")
#h20 randomforest-----------------------------------------------------------------------------
library(h2o)
h2o.init(nthreads=-1,max_mem_size='6G')
mytrainlog<-mytrain%>%mutate(logSales=log(Sales+1));myvalidlog<-myvalid%>%mutate(logSales=log(Sales+1))
rf.h2o <- h2o.randomForest(colnames(mytrainlog[,-c(1,3,4,17)]),"logSales", 
                           ntrees = 100,
                           max_depth = 30,
                           nbins_cats = 1115, ## allow it to fit store ID
                           training_frame=as.h2o(mytrainlog[,-c(1,3,4)]))

pred.h2<-as.data.frame(exp(h2o.predict(rf.h2o,as.h2o(myvalidlog[,-c(1,3,4)])))-1)
err.h2<-rmspe(myvalidlog$Sales,pred.h2)
err.h2

pred.f.h2<-as.data.frame(exp(h2o.predict(rf.h2o,as.h2o(test_open[,-c(1,2,4)])))-1)
open.result<-data.frame(Id=test_open$Id,pred.f.h2);colnames(open.result)<-c("Id","Sales")
h2o.result<-merge(data.frame(Id=test$Id),open.result,by="Id",all=TRUE) ; h2o.result[is.na(h2o.result)] <- 0 
write_csv(h2o.result, "h2orf2.csv")
#xgboost-----------------------------------------------------------------------------------
library(xgboost)
param <- list("objective" = "multi:softprob", 
              "eval_metric" = "mlogloss", 
              "num_class" = 10)
set.seed(1004)
clf <- xgboost(data        = data.matrix(mytrain[,-c(1,3,4)]), 
               label       = log(mytrain$Sales+1),
               objective           = "reg:linear", 
               booster = "gbtree",
               eta                 = 0.025, 
               max_depth           = 10, 
               subsample           = 0.7, 
               colsample_bytree    = 0.7, 
               min_child_weigh     = 10,
               nrounds             = 2000, 
               verbose             = 0,
               eval_metric = "rmse") 

clf2 <- xgboost(data        = data.matrix(mytrain[,-c(1,3,4)]), 
                label       = log(mytrain$Sales+1),
                objective           = "reg:linear", 
                booster = "gbtree",
                eta                 = 0.025, 
                max_depth           = 12, 
                subsample           = 0.9, 
                colsample_bytree    = 0.7, 
                min_child_weigh     = 10,
                nrounds             = 3000, 
                verbose             = 0,
                eval_metric = "rmse") 

clf3 <- xgboost(data        = data.matrix(mytrain[,-c(1,3,4)]), 
                label       = log(mytrain$Sales+1),
                objective           = "reg:linear", 
                booster = "gbtree",
                eta                 = 0.03, 
                max_depth           = 10, 
                subsample           = 0.9, 
                colsample_bytree    = 0.7, 
                min_child_weigh     = 10,
                nrounds             = 2500, 
                verbose             = 0,
                eval_metric = "rmse") 

pred.xg <- exp(predict(clf,data.matrix(myvalid[,-c(1,3,4)]),type='response'))-1
err<-rmspe(myvalid$Sales,pred.xg)
err#0.1907

pred.f.xg<-exp(predict(clf,data.matrix(test_open[,-c(1,2,4)]),type='response'))-1
open.result<-data.frame(Id=test_open$Id,Sales=pred.f.xg);
xg.result<-merge(data.frame(Id=test$Id),open.result,by="Id",all=TRUE) ; xg.result[is.na(xg.result)] <- 0 
write_csv(xg.result, "xg.csv")

pred.xg2 <- exp(predict(clf2,data.matrix(myvalid[,-c(1,3,4)]),type='response'))-1
err22<-rmspe(myvalid$Sales,pred.xg2)
err22#0.1823

pred.f.xg2<-exp(predict(clf2,data.matrix(test_open[,-c(1,2,4)]),type='response'))-1
open.result<-data.frame(Id=test_open$Id,Sales=pred.f.xg2)
xg.result2<-merge(data.frame(Id=test$Id),open.result,by="Id",all=TRUE) ; xg.result2[is.na(xg.result2)] <- 0 
write_csv(xg.result2, "xg2.csv")

pred.xg3 <- exp(predict(clf3,data.matrix(myvalid[,-c(1,3,4)]),type='response'))-1
err33<-rmspe(myvalid$Sales,pred.xg3)
err33#0.1875

pred.f.xg3<-exp(predict(clf3,data.matrix(test_open[,-c(1,2,4)]),type='response'))-1
open.result<-data.frame(Id=test_open$Id,Sales=pred.f.xg3)
xg.result3<-merge(data.frame(Id=test$Id),open.result,by="Id",all=TRUE) ; xg.result3[is.na(xg.result3)] <- 0 
write_csv(xg.result3, "xg3.csv")

glm1<-glm(log(Sales+1)~.,data=mytrain[,-c(1,4)],family=Gamma(link="inverse"))
step(glm2)
glm2<-glm(formula = log(Sales + 1) ~(DayOfWeek+Promo+ StateHoliday+SchoolHoliday+Year+ Month+Day+StoreType+Assortment+CompetitionDistance+Promo2+Competitionmthdiff+ Promo2wdiff )^2, family = Gamma(link = "inverse"), 
          data = mytrain[, -c(1, 4)])
pred.f.glm<-exp(predict(glm2,myvalid[,-c(1,3,4)],type="response"))-1
errglm<-rmspe(myvalid$Sales,pred.f.glm)
errglm

