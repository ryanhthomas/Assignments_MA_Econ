rm(list=ls())

library(ISLR)
set.seed(9004)
summary(OJ)

#(a)#
train_ind=sample(seq_len(nrow(OJ)),800)
train=OJ[train_ind,]
test=OJ[-train_ind,]

#(b)#
library(e1071)
svmfit.linear=svm(Purchase~.,data=train,kernel="linear",cost=0.01)
summary(svmfit.linear)

#(c)#
table(true=train$Purchase, pred=predict(svmfit.linear))
table(true=test$Purchase, pred=predict(svmfit.linear,newdata=test))

#(d)#
tune.out.linear=tune(svm, Purchase~.,data=train,kernel="linear",ranges=list(cost=c(0.01, 0.1, seq(0.25,10, 0.25))))
summary(tune.out.linear)
tune.out.linear$best.model

#(e)#
svmfit.best.linear=svm(Purchase~.,data=train,kernel="linear",cost=2.75)
table(true=train$Purchase, pred=predict(svmfit.best.linear))
table(true=test$Purchase, pred=predict(svmfit.best.linear,newdata=test))

#(f)#
svmfit.radial=svm(Purchase~.,data=train,kernel="radial",cost=0.01)
summary(svmfit.radial)
table(true=train$Purchase, pred=predict(svmfit.radial))
table(true=test$Purchase, pred=predict(svmfit.radial,newdata=test))
tune.out.radial=tune(svm, Purchase~.,data=train,kernel="radial",ranges=list(cost=c(0.01, 0.1, seq(0.25,10, 0.25))))
summary(tune.out.radial)
tune.out.radial$best.model
svmfit.best.radial=svm(Purchase~.,data=train,kernel="radial",cost=0.5)
table(true=train$Purchase, pred=predict(svmfit.best.radial))
table(true=test$Purchase, pred=predict(svmfit.best.radial,newdata=test))

#(g)#
svmfit.poly=svm(Purchase~.,data=train,kernel="polynomial",degree=2,cost=0.01)
summary(svmfit.poly)
table(true=train$Purchase, pred=predict(svmfit.poly))
table(true=test$Purchase, pred=predict(svmfit.poly,newdata=test))
tune.out.poly=tune(svm, Purchase~.,data=train,kernel="polynomial",degree=2,ranges=list(cost=c(0.01, 0.1, seq(0.25,10, 0.25))))
summary(tune.out.poly)
tune.out.poly$best.model
svmfit.best.poly=svm(Purchase~.,data=train,kernel="polynomial",degree=2,cost=5)
table(true=train$Purchase, pred=predict(svmfit.best.poly))
table(true=test$Purchase, pred=predict(svmfit.best.poly,newdata=test))

