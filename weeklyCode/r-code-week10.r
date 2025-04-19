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
auto <- auto[-c(19,50,74,75),] # remove outliers 

## Plot relation ship between the response and the engine.size
plot(auto$engine.size,auto$city.distance,xlab="engine size",ylab="city distance")

## Fit linear regression
fit.lm <- lm(city.distance~engine.size,data=auto)
summary(fit.lm)
abline(fit.lm$coef[1],fit.lm$coef[2],col=2,lwd=2)

# Estimate prediction error with cross-validation
K <- 10
N <- nrow(auto)
set.seed(1)
index <- sample(rep(1:K, ceiling(N/ K))[1:N])# split the data in approximately equisize folds
Err.lm <- 0
for(k in 1:K)
{
  fit.lm.k <- lm(city.distance~engine.size,data=auto,subset=(1:N)[-which(index == k)])
  Err.lm <- Err.lm+sum((auto$city.distance[index == k]-predict(fit.lm.k,auto[index == k,]))^2)
}
Err.lm <- Err.lm/N

## Fit local linear regression
fit.loess.1 <- loess(city.distance~engine.size,data=auto,degree=1)
summary(fit.loess.1)
lines(sort(auto$engine.size),predict(fit.loess.1,data.frame(engine.size=sort(auto$engine.size))),col=3,lwd=2)

# Estimate prediction error with cross-validation
Err.loess.1 <- 0
for(k in 1:K)
{
  fit.loess.1.k <- loess(city.distance~engine.size,data=auto,subset=(1:N)[-which(index == k)],degree=1)
  Err.loess.1 <- Err.loess.1+sum((auto$city.distance[index == k]-predict(fit.loess.1.k,auto[index == k,]))^2)
}
Err.loess.1 <- Err.loess.1/N

## Fit local quadratic regression
fit.loess.2 <- loess(city.distance~engine.size,data=auto,degree=2)
summary(fit.loess.2)
lines(sort(auto$engine.size),predict(fit.loess.2,data.frame(engine.size=sort(auto$engine.size))),col=4,lwd=2)

# Estimate prediction error with cross-validation
Err.loess.2 <- 0
for(k in 1:K)
{
  fit.loess.2.k <- loess(city.distance~engine.size,data=auto,subset=(1:N)[-which(index == k)],degree=2)
  Err.loess.2 <- Err.loess.2+sum((auto$city.distance[index == k]-predict(fit.loess.2.k,auto[index == k,]))^2)
}
Err.loess.2 <- Err.loess.2/N

c(Err.lm,Err.loess.1,Err.loess.2)

## Use of local regression in diagnostic plots: the
## red curves are loess estimates:
plot(fit.lm)

