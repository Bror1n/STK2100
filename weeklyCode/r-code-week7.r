# =============================================
# Prostate data
# =============================================

# Read the data
prostate <- read.csv("http://www.uio.no/studier/emner/matnat/math/STK2100/data/prostate.data",sep="\t",header=TRUE)
prostate <- prostate[,-1]
prostate$svi <- as.factor(prostate$svi)

head(prostate)

# Fit full model
fit.lm = lm(lpsa~.-train,data=prostate,subset=(train==TRUE))
summary(fit.lm)

library(glmnet)

# Standardise the numerical covariates
p <- 8
prostate.stand <- prostate[,-10] #remove indicator of training and test sets
for(j in 1:p)
{
  if(!is.factor(prostate[,j]))
  {
    prostate.stand[,j] <- (prostate.stand[,j]-mean(prostate.stand[,j]))/sd(prostate.stand[,j])
  }
}

# Make design matrix (without intercept)
X <- model.matrix(lpsa~.,data=prostate.stand)[,-1]
y <- prostate.stand$lpsa

## Ridge regression

# Fit ridge regression models for a range of lambda values
lambda.grid=10^seq(5,-2,length=100)
fit.ridge <- glmnet(X[prostate$train == TRUE,],y[prostate$train == TRUE],lambda=lambda.grid,alpha=0)
plot(fit.ridge,xvar="lambda")

rss.ridge <- rep(NA,length(lambda.grid)) #Calculate RSS for each value of lambda
for(k in 1:length(lambda.grid))
{
  pred.ridge <- predict(fit.ridge,s=lambda.grid[k],newx=X[prostate$train == FALSE,])
  rss.ridge[k] <- sqrt(mean((pred.ridge-y[prostate$train == FALSE])^2))
}
plot(log(lambda.grid),rss.ridge,type="l",xlab="log(lambda)")
abline(v=log(lambda.grid)[which.min(rss.ridge)],lty=2,col=2,lwd=2)


# Choose lambda with cross validation (K=10 is the default)
fit.ridge.cv <- cv.glmnet(X[prostate$train == TRUE,],y[prostate$train == TRUE],lambda=lambda.grid,alpha=0)
fit.ridge.cv
abline(v=log(fit.ridge.cv$lambda.min),lty=2,col=3,lwd=2)

plot(fit.ridge.cv)

fit.ridge.final <- glmnet(X[prostate$train == TRUE,],y[prostate$train == TRUE],lambda=fit.ridge.cv$lambda.min,alpha=0)
coef(fit.ridge.final)


## Lasso regression

# Fit Lasso regression models for a range of lambda values
lambda.grid=10^seq(5,-2,length=100)
fit.lasso <- glmnet(X[prostate$train == TRUE,],y[prostate$train == TRUE],lambda=lambda.grid,alpha=1)
plot(fit.lasso,xvar="lambda")

rss.lasso <- rep(NA,length(lambda.grid)) #Calculate RSS for each value of lambda
for(k in 1:length(lambda.grid))
{
  pred.lasso <- predict(fit.lasso,s=lambda.grid[k],newx=X[prostate$train == FALSE,])
  rss.lasso[k] <- sqrt(mean((pred.lasso-y[prostate$train == FALSE])^2))
}
plot(log(lambda.grid),rss.lasso,type="l",xlab="log(lambda)")
abline(v=log(lambda.grid)[which.min(rss.lasso)],lty=2,col=2,lwd=2)


# Choose lambda with cross validation (K=10 is the default)
fit.lasso.cv <- cv.glmnet(X[prostate$train == TRUE,],y[prostate$train == TRUE],lambda=lambda.grid,alpha=1)
fit.lasso.cv
abline(v=log(fit.lasso.cv$lambda.min),lty=2,col=3,lwd=2)

plot(fit.lasso.cv)

fit.lasso.final <- glmnet(X[prostate$train == TRUE,],y[prostate$train == TRUE],lambda=fit.lasso.cv$lambda.min,alpha=1)
coef(fit.lasso.final)


## PC regression

# Compute principal components
ind = c(1:4,6:8) #pick variables that are numeric
Z = prcomp(prostate[,ind],retx=TRUE,scale=TRUE)$x
prostate.pca = data.frame(Z=Z,svi=prostate$svi,lpsa=prostate$lpsa)

# Fit regression model to all the principal components
fit.pca = lm(lpsa~.,data=prostate.pca[prostate$train==TRUE,])
summary(fit.pca)

# Fit regression model to the principal components in decreasing order 
M <- ncol(Z)
rss.pca <- rep(0,M)
for(m in 1:M)
{
  prostate.pca = data.frame(Z=Z[,1:m],svi=prostate$svi,lpsa=prostate$lpsa)
  fit.pca = lm(lpsa~.,data=prostate.pca[prostate$train==TRUE,])
  pred.pca = predict(fit.pca,newdata=prostate.pca[prostate$train==FALSE,])
  rss.pca[m] = mean((prostate.pca$lpsa[prostate$train==FALSE]-pred.pca)^2)
}
plot(1:m,rss.pca)
abline(v=which.min(rss.pca),col=2,lty=2,lwd=2)

# =============================================
# Hitters data
# =============================================

# The data are available in the R package ISLR
library(ISLR)
data("Hitters")
Hitters=na.omit(Hitters) #remove missing values

# Standardise the numerical covariates
p <- 19
Hitters.stand <- Hitters #remove indicator of training and test sets
for(j in 1:(p+1))
{
  if((!is.factor(Hitters[,j]))&(names(Hitters)[j] != "Salary"))
  {
    Hitters.stand[,j] <- (Hitters.stand[,j]-mean(Hitters.stand[,j]))/sd(Hitters.stand[,j])
  }
}

# Make design matrix (without intercept)
X <- model.matrix(Salary~.,data=Hitters.stand)[,-1]
y <- Hitters.stand$Salary

#Dividing into training/test set
set.seed(1)
train=sample(1:nrow(X), nrow(X)/2)
test=(1:nrow(X))[-train]

## Ridge regression

# Fit ridge regression models for a range of lambda values
lambda.grid=10^seq(5,-2,length=100)
fit.ridge <- glmnet(X[train,],y[train],lambda=lambda.grid,alpha=0)
plot(fit.ridge,xvar="lambda")

rss.ridge <- rep(NA,length(lambda.grid)) #Calculate RSS for each value of lambda
for(k in 1:length(lambda.grid))
{
  pred.ridge <- predict(fit.ridge,s=lambda.grid[k],newx=X[test,])
  rss.ridge[k] <- sqrt(mean((pred.ridge-y[test])^2))
}
plot(log(lambda.grid),rss.ridge,type="l",xlab="log(lambda)")
abline(v=log(lambda.grid)[which.min(rss.ridge)],lty=2,col=2,lwd=2)


# Choose lambda with cross validation (K=10 is the default)
fit.ridge.cv <- cv.glmnet(X[train,],y[train],lambda=lambda.grid,alpha=0)
fit.ridge.cv
abline(v=log(fit.ridge.cv$lambda.min),lty=2,col=3,lwd=2)

plot(fit.ridge.cv)

fit.ridge.final <- glmnet(X[train,],y[train],lambda=fit.ridge.cv$lambda.min,alpha=0)
coef(fit.ridge.final)


## Lasso regression

# Fit Lasso regression models for a range of lambda values
lambda.grid=10^seq(5,-2,length=100)
fit.lasso <- glmnet(X[train,],y[train],lambda=lambda.grid,alpha=1)
plot(fit.lasso,xvar="lambda")

rss.lasso <- rep(NA,length(lambda.grid)) #Calculate RSS for each value of lambda
for(k in 1:length(lambda.grid))
{
  pred.lasso <- predict(fit.lasso,s=lambda.grid[k],newx=X[test,])
  rss.lasso[k] <- sqrt(mean((pred.lasso-y[test])^2))
}
plot(log(lambda.grid),rss.lasso,type="l",xlab="log(lambda)")
abline(v=log(lambda.grid)[which.min(rss.lasso)],lty=2,col=2,lwd=2)

# Choose lambda with cross validation (K=10 is the default)
fit.lasso.cv <- cv.glmnet(X[train,],y[train],lambda=lambda.grid,alpha=1)
fit.lasso.cv
abline(v=log(fit.lasso.cv$lambda.min),lty=2,col=3,lwd=2)

plot(fit.lasso.cv)

fit.lasso.final <- glmnet(X[train,],y[train],lambda=fit.lasso.cv$lambda.min,alpha=1)
coef(fit.lasso.final)

## PC regression

# Compute principal components
ind = c(1:13,16:18) #pick variables that are numeric
Z = prcomp(Hitters[,ind],retx=TRUE,scale=TRUE)$x
Hitters.pca = data.frame(Z=Z,League=Hitters$League,Division=Hitters$Division,NewLeague=Hitters$NewLeague,Salary=Hitters$Salary)

# Fit regression model to all the principal components
fit.pca = lm(Salary~.,data=Hitters.pca[train,])
summary(fit.pca)

# Fit regression model to the principal components in decreasing order 
M <- ncol(Z)
rss.pca <- rep(0,M)
for(m in 1:M)
{
  Hitters.pca = data.frame(Z=Z[,1:m],League=Hitters$League,Division=Hitters$Division,NewLeague=Hitters$NewLeague,Salary=Hitters$Salary)
  fit.pca = lm(Salary~.,data=Hitters.pca[train,])
  pred.pca = predict(fit.pca,newdata=Hitters.pca[test,])
  rss.pca[m] = mean((Hitters.pca$Salary[test]-pred.pca)^2)
}
plot(1:m,rss.pca)
abline(v=which.min(rss.pca),col=2,lty=2,lwd=2)

