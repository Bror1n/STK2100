# =============================================
# Credit data
# =============================================
#
# Summary: 
# A simulated data set containing information on ten 
# thousand customers. The aim here is to predict which 
# customers will default on their credit card debt.
#
# Data description: 
# The data set consists of the following 4 variables
# Response variable:
#  default: has defaulted on the credit card (Yes/No)
# Covariates:
#  income: annual income in dollars
#  balance: average balance that the customer has remaining on the 
#           credit card after making the monthly payment
#  Student: is a student (Yes/No)
#
#======================================================

## Read the data
library(ISLR)
data(Default)


## Plot the data
default01 <- rep(0,nrow(Default))
default01[Default$default == "Yes"] <- 1
plot(Default$balance,Default$income,pch=16,col=2+default01,cex=0.7)
par(mfrow=c(1,2))
boxplot(income~default,data=Default)
boxplot(balance~default,data=Default)
par(mfrow=c(1,1))

## Divide into test and train, making sure that the class balance is the sam ein both
set.seed(1)
ind1 <- which(Default$default == "Yes")
n1 <- length(ind1)
train.ind1 <- sample(ind1,size=ceiling(n1/2))
train.ind0 <- sample((1:nrow(Default))[-ind1],size=ceiling((nrow(Default)-n1)/2))
Default.train <- Default[c(train.ind1,train.ind0),]
Default.test <- Default[-c(train.ind1,train.ind0),]
Default.test$default <- as.numeric(Default.test$default)-1

## Fit logistic regression model
fit.logit <- glm(default~income+balance+student,family=binomial(link=logit),data=Default.train)
summary(fit.logit)
exp(fit.logit$coef[2:4])

# Predict probabilities for the test set
pred.logit <- predict(fit.logit,newdata=Default.test,type="response") # predicted probabilities
y.hat.logit <- as.numeric(pred.logit >= 0.5) # classifications

# Evaluate preformance
AIC(fit.logit)
BIC(fit.logit)
mean(as.numeric(y.hat.logit != Default.test$default)) # 0-1 loss
-2*mean(Default.test$default*log(pred.logit)+(1-Default.test$default)*log(1-pred.logit)) # deviance

## Fit LDA model
library(MASS)
fit.lda <- lda(default~income+balance+student,data=Default.train,method="moment") # method of moments
print(fit.lda)
fit.lda2 <- lda(default~income+balance+student,data=Default.train,method="mle") # MLE
print(fit.lda2)

# Predict probabilities for the test set
pred.obj <- predict(fit.lda,newdata=Default.test)
pred.lda <- pred.obj$posterior[,2] # predicted probabilities
y.hat.lda <- as.numeric(pred.obj$class)-1 # classifications

# Evaluate preformance
pred.obj.train <- predict(fit.lda,newdata=Default.train)
pred.train <- pred.obj.train$posterior[,2]
# AIC
-2*sum((as.numeric(Default.train$default)-1)*log(pred.train)+(1-(as.numeric(Default.train$default)-1))*log(1-pred.train))+2*(2+2*2+6)
# BIC
-2*sum((as.numeric(Default.train$default)-1)*log(pred.train)+(1-(as.numeric(Default.train$default)-1))*log(1-pred.train))+log(nrow(Default.train))*(2+2*3+6)
mean(as.numeric(y.hat.lda != Default.test$default)) # 0-1 loss
-2*mean(Default.test$default*log(pred.lda)+(1-Default.test$default)*log(1-pred.lda)) # deviance

## Fit QDA model
fit.qda <- qda(default~income+balance+student,data=Default.train,method="moment") # method of moments
print(fit.qda)
fit.qda2 <- qda(default~income+balance+student,data=Default.train,method="mle") # MLE
print(fit.qda2)

# Predict probabilities for the test set
pred.obj <- predict(fit.qda,newdata=Default.test)
pred.qda <- pred.obj$posterior[,2] # predicted probabilities
y.hat.qda <- as.numeric(pred.obj$class)-1 # classifications

# Evaluate preformance
pred.obj.train <- predict(fit.qda,newdata=Default.train)
pred.train <- pred.obj.train$posterior[,2]
# AIC
-2*sum((as.numeric(Default.train$default)-1)*log(pred.train)+(1-(as.numeric(Default.train$default)-1))*log(1-pred.train))+2*(2+2*2+2*6)
# BIC
-2*sum((as.numeric(Default.train$default)-1)*log(pred.train)+(1-(as.numeric(Default.train$default)-1))*log(1-pred.train))+log(nrow(Default.train))*(2+2*3+2*6)
mean(as.numeric(y.hat.lda != Default.test$default)) # 0-1 loss
-2*mean(Default.test$default*log(pred.lda)+(1-Default.test$default)*log(1-pred.lda)) # deviance

#=======================================================
# Alligator data
#=======================================================
#
# Summary: 
# These are data from a study of factors influencing the primary
# food choice of alligators. 219 alligators in four Florida lakes
# were captured. Then one registered their size and the food type
# that was predominant in volume in the stomachs of these
# alligators. For more details see Delany et al. (1999)
#
# Data description: 
# The data set consists of the following 7 variables
# Response variables:
#  y1: number of alligators having fish as their primary food choice 
#  y2: number of alligators having invertebrate as their primary food choice 
#  y3: number of alligators having reptiles as their primary food choice 
#  y4: number of alligators having birds as their primary food choice 
#  y5: number of alligators having other types as their primary food choice 
# Covariates:
#  lake: lake where the alligator was captured (1=Hancock, 2=Ocklawaha, 
#        3=Trafford, 4=George)
#  size: indicator of whether the alligator was smaller or larger than 2.3m
#
#=======================================================

# Read aligator data from the web:
alligators = read.table("http://www.stat.ufl.edu/~aa/glm/data/Alligators.dat", header = TRUE)
alligators

## Fit the model 
library(VGAM)

# We fit a baseline category logit model with size and lake as
# main effects and fish (y1) as the reference category:
fit.main=vglm(cbind(y2,y3,y4,y5,y1)~factor(size)+factor(lake),
              family=multinomial, data=alligators)
summary(fit.main)


