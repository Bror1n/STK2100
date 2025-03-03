# =============================================
# Yesterday data
# =============================================
#
# Summary: 
# These are simulated data, generated from an
# additive error model Y=f(x)+epsilon, with
# epsilons independent from N(0,0.01^2). The data
# set is divided into a training set with N=30,
# called yesterday, and an independent test set
# with 30 observations, called tomorrow, with the 
# same covariate values
#
# Data description: 
# The data set consists of the following 2 variables:
# Response variable:
#  y.yesterday: responses in the training set
#  y.tomorrow: responses in the test set
# Covariates:
#  x (same values for y.yesterday and y.tomorrow)
#
#======================================================

## Read the data
yesterday <- read.table("http://azzalini.stat.unipd.it/Book-DM/yesterday.dat", header = TRUE)
attach(yesterday)

## Plot the data
y.range <- range(c(y.yesterday, y.tomorrow))
plot(x, y.yesterday, ylim = y.range, col=2, pch = 16, ylab = "y", cex = 2)
points(x, y.tomorrow, ylim = y.range, col=4, pch = 17, ylab = "y", cex = 2)

## Fit a linear model with polynomial in x of order p, for p=1,...,23
polynomial <- function(p) lm(y.yesterday ~ poly(x,p)) 
polynomials <- vector("list",length=23)
for(p in 1:23)
{
  polynomials[[p]] <- polynomial(p)
}

## Plot models for p=1,5,8,23
lines(x,polynomials[[1]]$fitted.values,col=2,lwd=2)
lines(x,polynomials[[5]]$fitted.values,col=3,lwd=2)
lines(x,polynomials[[8]]$fitted.values,col=7,lwd=2)
lines(x,polynomials[[23]]$fitted.values,col=8,lwd=2)

## Compute training and test error
error.train <- error.test <- rep(0,23)
for(p in 1:23)
{
  error.train[p] <- sum((y.yesterday-polynomials[[p]]$fitted.values)^2)
  error.test[p] <- sum((y.tomorrow-polynomials[[p]]$fitted.values)^2)
}
plot(1:23,error.train,type="l",lwd=2)
lines(1:23,error.test,col=2,lwd=2)
legend(16,0.015,c("Training error","Test error"),col=1:2,lty=rep(1,2))

## Compute bias and variance
sigma.eps <- 0.01 
N <- 30
source("http://www.uio.no/studier/emner/matnat/math/STK2100/v25/undervisningsmateriell/r-kode/f_true.r") #evaluation of the true f in x
polynomial.true <- function(p) lm(f.true ~ poly(x,p)) 
polynomials.true <- vector("list",length=23)
for(p in 1:23)
{
  polynomials.true[[p]] <- polynomial.true(p)
}
bias2 <- variance <- rep(0,23)
for(p in 1:23)
{
  bias2[p] <- mean((fitted(polynomials.true[[p]]) - f.true)^2)
  variance[p] <- sigma.eps^2*p/N
}
err <- bias2 + variance
min.err <- which.min(err)
plot(bias2, type = "b", col = 4, ylab = "Error", xlab = "Model complexity", cex = 2, pch = 16)
lines(variance, type = "b", col = 2, cex = 2, pch = 17)
lines(err, type = "b", cex = 2, pch = 18)
abline(v=min.err,lty=3,lwd=2)
legend('topright', legend = c("Bias^2", "Variance", "Total"), col = c(4, 2, 1), pch = c(16, 17, 18))

## Compute information criteria: AIC, BIC

# Refit with all the data
y.all <- c(y.yesterday,y.tomorrow)
x.all <- rep(x,2)
polynomials.all <- vector("list",length=23)
for(p in 1:23)
{
  polynomials.all[[p]] <- lm(y.all~poly(x.all,p))
}
AICvec <- BICvec <- rep(0,23)
for(p in 1:23)
{
  AICvec[p] <- AIC(polynomials.all[[p]])
  BICvec[p] <- BIC(polynomials.all[[p]])
}
min.AIC <- which.min(AICvec)
min.BIC <- which.min(BICvec)
plot(1:23, AICvec, ylim=c(min(AICvec, BICvec), max(AICvec, BICvec)), col = 2, type = "b", pch = 16, cex = 2, xlab = "Model complexity", ylab = "AIC and BIC criterion")
lines(BICvec, col = 4, type = "b", pch = 18, cex = 2)
abline(v=min.AIC,lty=3,col=2)
abline(v=min.BIC,lty=3,col=4)
legend(20,-270,legend=c("AIC","BIC"),pch=c(16,18),col=c(2,4))

## Cross-validation

# LOOCV
N <- length(y.all)
print(N)
Err.loocv <- rep(0,23)
for(p in 1:23)
{
  for(i in 1:N)
  {
    polynomials.loocv <- lm(y.all~poly(x.all,p),subset=(1:N)[-i])
    Err.loocv[p] <- Err.loocv[p]+(y.all[i]-predict(polynomials.loocv,data.frame(x.all=x.all[i])))^2/N
  }
}

# K-fold CV
Err.Kcv <- rep(0,23)
set.seed(20200219) # set seed, useful for data replication
K <- 10 # set the number of folds
index <- sample(rep(1:K, ceiling(N/ K))[1:N])# split the data in approximately equisize folds
for(p in 1:23)
{
  for(k in 1:K)
  {
    polynomials.Kcv <- lm(y.all~poly(x.all,p),subset=(1:N)[-which(index == k)])
    for(i in which(index==k))
    {
      Err.Kcv[p] <- Err.Kcv[p]+(y.all[i]-predict(polynomials.Kcv,data.frame(x.all=x.all[i])))^2/N
    }
  }
}

min.loocv <- which.min(Err.loocv)
min.Kcv <- which.min(Err.Kcv)
plot(1:23, Err.loocv, ylim=c(min(Err.loocv, Err.Kcv), max(Err.loocv, Err.Kcv)), col = 2, type = "b", pch = 16, cex = 2, xlab = "Model complexity", ylab = "Leave-one-out and 10-fold CV")
lines(1:23,Err.Kcv, col = 4, type = "b", pch = 18, cex = 2)
abline(v=min.loocv,lty=3,col=2)
abline(v=min.Kcv,lty=3,col=4)
legend(19,5.8e-04,legend=c("LOOCV","10-fold CV"),pch=c(16,18),col=c(2,4))

## Bootstrap 
B <- 1000
error.train.all <- rep(0,23)
for(p in 1:23)
{
  error.train.all[p] <- sum(polynomials.all[[p]]$residuals^2)
}
Err.boot.1 <- rep(0, 11) # just for the first 10 degrees of the polynomial
n.test <- 0
for (b in 1:B)
{
  set.seed(b) #set seed, useful for data replication
  index <- sample(N, replace = TRUE)
  temp.train.data <- data.frame(y=y.all[index], x=x.all[index]) #the selected observations form the training set
  temp.test.data <- data.frame(y=y.all[-index], x=x.all[-index]) #the rest the test set
  n.test <- n.test+nrow(temp.test.data)
  for(p in 1:11)
  {
    polynomials.boot <- lm(y~poly(x,p),data=temp.train.data)# fit the model on the training
    Err.boot.1[p] <- Err.boot.1[p]+sum((temp.test.data$y-predict(polynomials.boot,newdata=temp.test.data))^2)
  }
}
Err.boot.1 <- Err.boot.1/n.test
min.boot.1 <- which.min(Err.boot.1)
Err.boot.632 <- 0.368*error.train.all[1:11] + 0.632*Err.boot.1 #0.632 bootstrap
min.boot.632 <- which.min(Err.boot.632)
plot(1:11, Err.boot.1, ylim=c(min(Err.boot.1,Err.boot.632),max(Err.boot.1,Err.boot.632)),type = "b", xlab = "Model complexity", col = 2, pch = 16, ylab = "Error", cex = 2, lwd = 3)
lines(1:11, Err.boot.632, type = "b", pch = 18, col=4, lwd = 3, cex=2)
abline(v=min.boot.1,lty=3,col=2,lwd=2)
abline(v=min.boot.632,lty=3,col=4,lwd=2)
legend(9,0.011,c("Err_boot^(1)","Err_boot^(0.632)"),pch=c(16,18),col=c(2,4))


