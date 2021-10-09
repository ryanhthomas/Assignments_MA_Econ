#set up
.libPaths("C:/Users/vanvliec/Documents")
setwd("C:/Users/vanvliec/Documents")
install.packages("ISLR","C:/Users/vanvliec/Documents")
install.packages("gbm","C:/Users/vanvliec/Documents")
install.packages("np","C:/Users/vanvliec/Documents")
install.packages("class","C:/Users/vanvliec/Documents")

library("np")
library(gbm)
library(ISLR) 
library(class)

set.seed(342) 
summary(Caravan)
c<-Caravan
c$Purchase<-ifelse(c$Purchase=="Yes",1,0)
summary(c$Purchase)

#a
#create test and training sets
train<-c[1:1000,]
test<-c[1001:5822,]
#b
boost.caravan=gbm(Purchase~.,data=train,n.trees=1000,shrinkage=.01)
summary(boost.caravan)
par(mfrow=c(1,2))
plot(boost.caravan,i="PPERSAUT")
plot(boost.caravan,i="MKOOPKLA")
plot(boost.caravan,i="MOPLHOOG")

#c
#boosting model
yhat.boost=predict(boost.caravan,newdata=test,distribution="bernoulli",type="response",n.trees=1000)
summary(yhat.boost)
plot(yhat.boost)

boost.YesNo=rep("No",4822)
boost.YesNo[yhat.boost>=.2]="yes"
Original=rep("No",4822)
Original[test$Purchase==1]="yes"
table(boost.YesNo,Original)

summary(yhat.boost.pred)

#knn
rm(list = ls())

attach(Caravan)

standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test=1:1000
train.X=standardized.X[test,]
test.X=standardized.X[-test,]
train.Y=Purchase[test]
test.Y=Purchase[-test]
set.seed(342)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")


table(knn.pred,test.Y)

knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)


knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)


#Logistic
rm(list=ls())
attach(Caravan)

c<-Caravan
c$Purchase<-ifelse(c$Purchase=="Yes",1,0)
train<-c[1:1000,]
test<-c[1001:5822,]
Original=rep("No",4822)
Original[test$Purchase==1]="yes"

logitm<-glm(Purchase~.,family=binomial(link=logit),data=train)
yhat.logitm=predict(logitm,newdata=test,type="response")
summary(yhat.logitm)
log.YesNo=rep("No",4822)
log.YesNo[yhat.logitm>.2]="yes"
table(log.YesNo,Original)

