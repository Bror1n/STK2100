# =============================================
# Auto data
# =============================================
#
# Summary: 
# Data used in the book:
#    "Data anlysis and data mining" by A.Azzalini and B.Scarpa,
#    © Oxford University Press, 2012 (ISBN 978-0-19-976710-6).
#
# Data obtained by manipulation of the data available at
# ftp://ftp.ics.uci.edu/pub/machine-learningdatabases/autos
#
# Data description: 
# The data set consists of the following 18 variables:
# Response variable:
#  city.distance: city distance covered (km/l)
# Covariates:
#  make:                manufacturer (factor, 22 levels)    
#  fuel.type:           type of engine fuel (factor, 2 levels: diesel, gasoline)   
#  aspiration:          type of engine aspiration (factor, 2 levels: standard, turbo)   
#  body.style:          type of body style (factor, 5 levels: hardtop, wagon, sedan, hatchback, convertible)  
#  drive.wheels:        type of drive wheels (factor,  3 levels: 4wd, fwd, rwd)   
#  engine.location:     location of engine (factor, 2 levels: front, rear)  
#  wheel.base:          distance between axes (cm)   
#  length:              length (cm)    
#  width:               width (cm)  
#  height:              height (cm)   
#  curb.weight:         weight (kg)   
#  engine.size:         engine size (l)   
#  compression.rate:    compression rate   
#  hp:                  horse power   
#  peak-rpm:            number of peak revolutions per minute   
#  highway.distance:    highway distance (km/l)  
#  n.cylinders:         number of cylinders
#
#======================================================

## Read the data
auto <- read.table("http://azzalini.stat.unipd.it/Book-DM/auto.dat", header = TRUE)

## Fit a an additive model
library(gam)
fit.gam <- gam(city.distance~s(engine.size)+s(curb.weight)+s(HP)+factor(aspiration),data=auto)
par(mfrow=c(1,3))
plot(fit.gam,se=TRUE,terms=c("s(engine.size)","s(curb.weight)","s(HP)"))
summary(fit.gam)

## Fit a linear model and compare
fit.lm <- lm(city.distance ~ engine.size+curb.weight+HP+factor(aspiration),data=auto)
summary(fit.lm) 
anova(fit.lm,fit.gam,test="F")

## Fit a regression tree
library(tree)
set.seed(34534)
N = nrow(auto)
train = sample(1:N,101,replace=FALSE)

fit.t1 <- tree(city.distance~engine.size+curb.weight,data=auto[train,])
plot(fit.t1)
text(fit.t1, digits=2, pretty=3,cex=0.5)
pred.t1 = predict(fit.t1,auto[-train,])
sum((auto$city.distance[-train]-pred.t1)^2)

# Prune the tree
fit.p1<- prune.tree(fit.t1, newdata=auto[-train,])
plot(fit.p1)
fit.p1.best <- prune.tree(fit.t1, newdata=auto[-train,],best=6) #Get the best tree
plot(fit.p1.best)
text(fit.p1.best, digits=2, pretty=3,cex=0.5)
pred.p1 = predict(fit.p1.best,auto[-train,])
sum((auto$city.distance[-train]-pred.p1)^2)

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

# Transform input data due to long-tailed distribution
spam[,1:57] = log(spam[,1:57]+0.1)

## Fit logistic GAM with all covariates
library(gam)
nam = names(spam)[1:57]
k = 57
formula = as.formula(paste("y",paste(paste("s(",nam[1:k],")",sep=""),collapse="+"), sep="~"))
fit.gam = gam(formula,data=spam,subset=spam$train,family=binomial)
pred.gam = predict(fit.gam,spam[!spam$train,],type="response")>0.5
err.gam=mean(spam$y[!spam$train]!=pred.gam)

# Compare with the ordinary logistic regression model
fit = glm(y~.-train,data=spam,family=binomial,subset=spam$train)
pred = predict(fit,spam[!spam$train,],type="response")>0.5
err.logist=mean(spam$y[!spam$train]!=pred)

c(err.gam,err.logist)
summary(fit.gam)

# Using variables selected in textbook
ind = c(5:8,16:17,25:27,37,45:46,52:53,56:57)

formula = as.formula(paste("y",paste(paste("s(",nam[ind],")",sep=""),collapse="+"), sep="~"))
fit.gam.sub = gam(formula,data=spam,subset=spam$train,family=binomial)
pred.gam.sub = predict(fit.gam.sub,spam[!spam$train,],type="response")>0.5
err.gam.sub=mean(spam$y[!spam$train]!=pred.gam.sub)

formula = as.formula(paste("y",paste(paste(nam[ind],sep=""),collapse="+"), sep="~"))
fit.glm.sub = glm(formula,data=spam,subset=spam$train,family=binomial)
pred.glm.sub = predict(fit.glm.sub,spam[!spam$train,],type="response")>0.5
err.glm.sub=mean(spam$y[!spam$train]!=pred.glm.sub)

c(err.gam.sub,err.glm.sub)
summary(fit.gam.sub)

# Replace some of the terms with linear effects
ind.smooth <- c(6:7,17,27,37,45:46,52:53,56)
ind.lin <- c(5,8,16,25:26,57)
formula = as.formula(paste("y",paste(c(paste("s(",nam[ind.smooth],")",sep=""),nam[ind.lin]),collapse="+"), sep="~"))
fit.gam.sub.lin = gam(formula,data=spam,subset=spam$train,family=binomial)
pred.gam.sub.lin = predict(fit.gam.sub.lin,spam[!spam$train,],type="response")>0.5
err.gam.sub.lin = mean(spam$y[!spam$train]!=pred.gam.sub.lin)

c(err.gam.sub.lin,err.glm.sub)
summary(fit.gam.sub.lin)

## Fit a classification tree to all covariates
library(tree)
formula = as.formula(paste("factor(y)",paste(nam,collapse="+"), sep="~"))
fit.t1 <- tree(formula,data=spam,subset=spam$train,split="deviance")
plot(fit.t1)
text(fit.t1, digits=2, pretty=3,cex=0.5)
pred.t1 = predict(fit.t1,spam[!spam$train,],type="class")
err.t1 = mean(spam$y[!spam$train]!=pred.t1)
err.t1

# Prune the tree
fit.p1 <- prune.tree(fit.t1, newdata=spam[spam$train == FALSE,])
plot(fit.p1) # No gain by pruning

# Using variables selected in textbook
ind = c(5:8,16:17,25:27,37,45:46,52:53,56:57)
formula = as.formula(paste("factor(y)",paste(nam[ind],collapse="+"), sep="~"))
fit.t2 <- tree(formula,data=spam,subset=spam$train,split="deviance")
plot(fit.t2)
text(fit.t2, digits=2, pretty=3,cex=0.5)
pred.t2 = predict(fit.t2,spam[!spam$train,],type="class")
err.t2 = mean(spam$y[!spam$train]!=pred.t2)
err.t2 

