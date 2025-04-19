# =============================================
# E-mail spam data
# =============================================
#
# Summary: 
# Information from 4601 e-mail messages with the purpose of 
# constructing an automatic spam detector
#
# Data description: 
# The data set consists of the following 59 variables:
# Response variable:
#  y: indicator of whether the e-mail is spam True/False
# Covariates:
#  x1-x57: 57 of the most  commonly occuring words and 
#          punctuation marks in the e-mails
# Indicator of training and test sets:
#  train: indicator of whether to use as training or test sample
#
#===============================================

## Read data
spam = read.table("http://www.uio.no/studier/emner/matnat/math/STK2100/data/spam_data.txt",header=TRUE)
x.tr <- matrix(unlist(spam[spam$train,1:57]),nrow=sum(spam$train),ncol=57)
y.tr <- unlist(spam$y[spam$train])
x.te <- matrix(unlist(spam[!spam$train,1:57]),nrow=sum(!spam$train),ncol=57)
y.te <- unlist(spam$y[!spam$train])

## Fit boosting trees to the data, first with stumps (J=2) 
## as the weak learner
library(xgboost)
# Use 10-fold cross-validation to select M and the learning rate is set to 0.1
fit.boost.1.cv <- xgb.cv(data=xgb.DMatrix(data=x.tr,label=y.tr),nrounds = 1000,eta=0.1,nfold = 10,metrics=list("error"),max_depth=1,objective="binary:logistic")
best.boost.1 <- which.min(fit.boost.1.cv$evaluation_log$test_error_mean)
plot(fit.boost.1.cv$evaluation_log$train_error_mean,type="l",lwd=2,xlab="Stage",ylab="Misclassification error")
lines(fit.boost.1.cv$evaluation_log$test_error_mean,col=2,lwd=2)
abline(v=best.boost.1,lty=2,lwd=2)
legend(350,0.2,c("training","test"),col=c(1,2),lty=rep(1,2))
fit.boost.1 <- xgboost(data=x.tr,label=y.tr,max.depth=1,nrounds=best.boost.1,eta=0.1,objective="binary:logistic")
pred.boost.1 <- predict(fit.boost.1,newdata=x.te)
yhat.boost.1 <- pred.boost.1>0.5
err.boost.1 <- mean(y.te != yhat.boost.1)



## Fit boosting trees to the data, first with trees depth 2 (J=4) 
## as the weak learner
# Use 10-fold cross-validation to select M and the learning rate is set to 0.1
fit.boost.2.cv <- xgb.cv(data=xgb.DMatrix(data=x.tr,label=y.tr),nrounds = 500,eta=0.1,nfold = 10,metrics=list("error"),max_depth=2,objective="binary:logistic")
best.boost.2 <- which.min(fit.boost.2.cv$evaluation_log$test_error_mean)
plot(fit.boost.2.cv$evaluation_log$train_error_mean,type="l",lwd=2,xlab="Stage",ylab="Misclassification error")
lines(fit.boost.2.cv$evaluation_log$test_error_mean,col=2,lwd=2)
abline(v=best.boost.2,lty=2,lwd=2)
legend(350,0.14,c("training","test"),col=c(1,2),lty=rep(1,2))
fit.boost.2 <- xgboost(data=x.tr,label=y.tr,max.depth=2,nrounds=best.boost.2,eta=0.1,objective="binary:logistic")
pred.boost.2 <- predict(fit.boost.2,newdata=x.te)
yhat.boost.2 <- pred.boost.2>0.5
err.boost.2 <- mean(y.te != yhat.boost.2)

## Fit boosting trees to the data, first with trees of depth 3 (J=8) 
## as the weak learner
# Use 10-fold cross-validation to select M and the learning rate is set to 0.1
fit.boost.3.cv <- xgb.cv(data=xgb.DMatrix(data=x.tr,label=y.tr),nrounds = 500,eta=0.1,nfold = 10,metrics=list("error"),max_depth=3,objective="binary:logistic")
best.boost.3 <- which.min(fit.boost.3.cv$evaluation_log$test_error_mean)
plot(fit.boost.3.cv$evaluation_log$train_error_mean,type="l",lwd=2,xlab="Stage",ylab="Misclassification error")
lines(fit.boost.3.cv$evaluation_log$test_error_mean,col=2,lwd=2)
abline(v=best.boost.3,lty=2,lwd=2)
legend(350,0.105,c("training","test"),col=c(1,2),lty=rep(1,2))
fit.boost.3 <- xgboost(data=x.tr,label=y.tr,max.depth=3,nrounds=best.boost.3,eta=0.1,objective="binary:logistic")
pred.boost.3 <- predict(fit.boost.3,newdata=x.te)
yhat.boost.3 <- pred.boost.3>0.5
err.boost.3 <- mean(y.te != yhat.boost.3)

c(err.boost.1,err.boost.2,err.boost.3)
