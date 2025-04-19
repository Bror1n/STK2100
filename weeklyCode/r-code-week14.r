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
# The reponse vector must be given as a factor for R to understand that this 
# is a classification problem
y.tr <- as.factor(unlist(spam$y[spam$train])) 
x.te <- matrix(unlist(spam[!spam$train,1:57]),nrow=sum(!spam$train),ncol=57)
y.te <- as.factor(unlist(spam$y[!spam$train]))

## Fit a random forest with m=2 and B=2500
library(randomForest)
m <- 2
B <- 2500
fit.rf.1 <- randomForest(x=x.tr,y=y.tr,ntree=B,mtry=m)
yhat.rf.1 <- predict(fit.rf.1,newdata=x.te) # these are classifications
err.rf.1 <- mean(y.te != yhat.rf.1)
# Plot the OOB error estimate accumulated over the trees as a function of
# the number of trees up to that point
plot(fit.rf.1$err.rate[,1],type="l",ylim=c(0.04,0.12),lwd=2,xlab="Number of trees",ylab="OOB error estimate")

## Fit a random forest with m=6 and B=2500
m <- 6
B <- 2500
fit.rf.2 <- randomForest(x=x.tr,y=y.tr,ntree=B,mtry=m)
yhat.rf.2 <- predict(fit.rf.2,newdata=x.te) # these are classifications
err.rf.2 <- mean(y.te != yhat.rf.2)
lines(fit.rf.2$err.rate[,1],lwd=2,col=2)

## Fit a random forest with m=10 and B=2500
m <- 10
B <- 2500
fit.rf.3 <- randomForest(x=x.tr,y=y.tr,ntree=B,mtry=m)
yhat.rf.3 <- predict(fit.rf.3,newdata=x.te) 
err.rf.3 <- mean(y.te != yhat.rf.3)
lines(fit.rf.3$err.rate[,1],lwd=2,col=3)
legend(2000,0.12,c("m=2","m=6","m=10"),col=1:3,lty=rep(1,3))

c(err.rf.1,err.rf.2,err.rf.3)
