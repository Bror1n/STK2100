# =============================================
# Advertising data
# =============================================
#
# Summary: 
# These are records of sales and advertising budgets
# from 200 different markets
#
# Data description: 
# The data set consists of the following 4 variables:
# Response variable:
#  Sales: sales in thousands of units
# Covariates:
#  TV: budget for advertising on TV in thousands of dollars 
#  Radio: budget for advertising on radio in thousands of dollars 
#  Newspaper: budget for advertising in newspapers in thousands of dollars 
#
#======================================================

# Read the data
advertising <- read.csv("http://www.uio.no/studier/emner/matnat/math/STK2100/data/advertising.csv")

# Fit a model with all 3 covariates
fit.lm <- lm(Sales~TV+Radio+Newspaper,data=advertising)
summary(fit.lm)

# T-test for the effect of Newspaper
N <- nrow(advertising)
p <- 3
beta.hat <- coef(fit.lm)[4]
sigma.hat <- sqrt(sum(fit.lm$residuals^2)/(N-p-1))
sigma.hat
summary(fit.lm)$sigma
X <- model.matrix(fit.lm)
v <- solve(t(X)%*%X)
se.beta.hat <- sigma.hat*sqrt(v[4,4])
se.beta.hat  
summary(fit.lm)$coef[4,2]
tobs <- beta.hat/se.beta.hat #observed t-statistic
tobs
df <- N-p-1
qt(0.975,df) #rejection level 
2*(1-pt(abs(tobs),df)) #P-value

# F-test for the effects of all 3 covariates
fit.null <- lm(Sales~1,data=advertising) #fit null model
RSS <- sum(fit.lm$residuals^2)
RSS0 <- sum(fit.null$residuals^2)
q <- 3
Fobs <- ((RSS0-RSS)/q)/(RSS/(N-p-1))
Fobs
qf(0.95,q,N-p-1) #rejection level 
1-pf(Fobs,q,N-p-1) #P-value

anova(fit.null,fit.lm)

# F-test for the effect of Newspaper
fit.lm2 <- lm(Sales~TV+Radio,data=advertising)
RSS <- sum(fit.lm$residuals^2)
RSS0 <- sum(fit.lm2$residuals^2)
q <- 1
Fobs <- ((RSS0-RSS)/q)/(RSS/(N-p-1))
Fobs
qf(0.95,q,N-p-1) #rejection level 
1-pf(Fobs,q,N-p-1) #P-value

anova(fit.lm2,fit.lm)

# F-test for the effect of Newspaper and Radio
fit.lm3 <- lm(Sales~TV,data=advertising)
RSS <- sum(fit.lm$residuals^2)
RSS0 <- sum(fit.lm3$residuals^2)
q <- 2
Fobs <- ((RSS0-RSS)/q)/(RSS/(N-p-1))
Fobs
qf(0.95,q,N-p-1) #rejection level 
1-pf(Fobs,q,N-p-1) #P-value

anova(fit.lm3,fit.lm)

#Confidence interval for the betas
beta.hat+se.beta.hat*qt(c(0.025,0.975),N-p-1)

confint(fit.lm,level=0.95)

#Confidence interval for E[Y|x] for a new x
newdata <- data.frame(TV=100,Radio=20,Newspaper=50)
y.hat <- predict(fit.lm,newdata)
y.hat
x <- c(1,100,20,50)
se.y.hat <- as.numeric(sigma.hat*sqrt(t(x)%*%v%*%x))
y.hat+se.y.hat*qt(c(0.025,0.975),N-p-1)
predict(fit.lm,newdata,interval="confidence")

#Prediction interval for Y for a new x
y.hat+sqrt(sigma.hat^2+se.y.hat^2)*qt(c(0.025,0.975),N-p-1)
predict(fit.lm,newdata,interval="predict")

# =============================================
# Credit data
# =============================================
#
# Summary: 
# These are records of the average credit debt and
# for 400 customers, as well as diverse information 
# about the customers, such as age, gender, credit 
# rating, and so on
#
# Data description: 
# The data set consists of the following 10 variables
# (the first colums contains the number of the individual):
# Response variable:
#  Balance: average credit debt
# Covariates:
#  Income: income in thousands of dollars
#  Limit: credit limit
#  Rating: credit rating
#  Cards: number of credit cards
#  Age: age in years
#  Education: years of education
#  Gender: gender 
#  Student: is a student (Yes/No)
#  Married: is married (Yes/No)
#  Ethnicity: ethnicity (Caucasian/African American/Asian) 
#
#======================================================

# Read the data
credit <- read.csv("http://www.uio.no/studier/emner/matnat/math/STK2100/data/credit.csv")

# Box plots of Balance for the qualitative covariates Gender, Student, Status and Ethnicity
par(mfrow=c(2,2))
boxplot(Balance~factor(Gender),data=credit)
boxplot(Balance~factor(Student),data=credit)
boxplot(Balance~factor(Married),data=credit)
boxplot(Balance~factor(Ethnicity),data=credit)
par(mfrow=c(1,1))

# Fit individual models with Income and each of the qualitative 
# covariates Student and Ethnicity
fit.income <- lm(Balance~Income,data=credit)
summary(fit.income)
fit.student <- lm(Balance~Income+factor(Student),data=credit)
summary(fit.student)
plot(credit$Income,credit$Balance)
abline(fit.income$coef[1],fit.income$coef[2],col='green',lwd=2)
abline(fit.student$coef[1],fit.student$coef[2],col='red',lwd=2)
abline(fit.student$coef[1]+fit.student$coef[3],fit.income$coef[2],col='blue',lwd=2)

fit.ethnicity <- lm(Balance~Income+factor(Ethnicity),data=credit)
summary(fit.ethnicity)
plot(credit$Income,credit$Balance)
abline(fit.income$coef[1],fit.income$coef[2],col='green',lwd=2)
abline(fit.ethnicity$coef[1],fit.ethnicity$coef[2],col='red',lwd=2)
abline(fit.ethnicity$coef[1]+fit.ethnicity$coef[3],fit.ethnicity$coef[2],col='blue',lwd=2)
abline(fit.ethnicity$coef[1]+fit.ethnicity$coef[4],fit.ethnicity$coef[2],col='purple',lwd=2)

# Zoomm in to see the differences
plot(credit$Income,credit$Balance,xlim=c(40,50),ylim=c(488,550))
abline(fit.income$coef[1],fit.income$coef[2],col='green',lwd=2)
abline(fit.ethnicity$coef[1],fit.ethnicity$coef[2],col='red',lwd=2)
abline(fit.ethnicity$coef[1]+fit.ethnicity$coef[3],fit.ethnicity$coef[2],col='blue',lwd=2)
abline(fit.ethnicity$coef[1]+fit.ethnicity$coef[4],fit.ethnicity$coef[2],col='purple',lwd=2)

#===================================================
# Advertising data
#===================================================

# Read the data
advertising <- read.csv("http://www.uio.no/studier/emner/matnat/math/STK2100/data/advertising.csv")

# Fit a model with TV and Radio
fit.lm2 <- lm(Sales~TV+Radio,data=advertising)

# Model with interaction effect
fit.lm4 <- lm(Sales~TV+Radio+TV:Radio,data=advertising)
summary(fit.lm4)
anova(fit.lm2,fit.lm4)

# =============================================
# Prostate data
# =============================================
#
# Summary: 
# This data set comes from the study by Stamey
# et al. (1989), that examined the relationship
# between the level of prostate spacific antigen
# (PSA) in 97 men who were about to receive
# a radical prostatectomy. 
#
# Data description: 
# The data set consists of the following 9 variables:
# Response variable:
#  lpsa: log of PSA
# Covariates:
#  lcavol: log cancer volume
#  lweight: log prostate weight
#  age: age in years
#  lbph: log of benignprostatic hyperplasia amount
#  svi: seminal vesicle invasion
#  lcp: log of capsular penetration
#  gleason: Gleason score
#  pgg45: percent of Gleason scores 4 and 5
#
#======================================================

# Read the data
prostate <- read.csv("http://www.uio.no/studier/emner/matnat/math/STK2100/data/prostate.data",sep="\t",header=TRUE)
prostate <- prostate[,-1]

# Fit full model
fit.lm = lm(lpsa~.-train,data=prostate,subset=(train==TRUE))
summary(fit.lm)

library(leaps)

### Forward selection
nvmax=8
regfit.fwd = regsubsets(lpsa~.-train,data=subset(prostate,train==TRUE),nvmax=nvmax,method="forward")
summary.regfit.fwd <- summary(regfit.fwd)
show(summary.regfit.fwd)

# R^2 when including each of the covariates
plot(regfit.fwd,scale="r2") 

#R^2 and R_adj^2 against the no. of cov. in the model
matplot(cbind(summary.regfit.fwd$rsq,summary.regfit.fwd$adjr2),type="l") 

#C_p, AIC and BIC against the no. of cov. in the model
summary.regfit.fwd$aic = summary.regfit.fwd$bic-log(n)*(c(1:nvmax)+2)+2*(c(1:nvmax)+2)
matplot(cbind(summary.regfit.fwd$cp,summary.regfit.fwd$aic,summary.regfit.fwd$bic),type="l",lty=1)
p.Cp = which.min(summary.regfit.fwd$cp)
p.aic = which.min(summary.regfit.fwd$aic)
p.bic = which.min(summary.regfit.fwd$bic)
points(c(p.Cp,p.aic,p.bic),
       c(summary.regfit.fwd$cp[p.Cp],summary.regfit.fwd$aic[p.aic],summary.regfit.fwd$bic[p.bic]))
legend("topright",c("C_p","AIC","BIC"),lty=1,col=1:3,)

### Backward selection
nvmax=8
regfit.bwd = regsubsets(lpsa~.-train,data=subset(prostate,train==TRUE),nvmax=nvmax,method="backward")
summary.regfit.bwd <- summary(regfit.bwd)
show(summary.regfit.bwd)

print(summary.regfit.fwd)
print(summary.regfit.bwd)


# R^2 when including each of the covariates
plot(regfit.bwd,scale="r2") 

#R^2 and R_adj^2 against the no. of cov. in the model
matplot(cbind(summary.regfit.bwd$rsq,summary.regfit.bwd$adjr2),type="l") 

#C_p, AIC and BIC against the no. of cov. in the model
summary.regfit.bwd$aic = summary.regfit.bwd$bic-log(n)*(c(1:nvmax)+2)+2*(c(1:nvmax)+2)
matplot(cbind(summary.regfit.bwd$cp,summary.regfit.bwd$aic,summary.regfit.bwd$bic),type="l",lty=1)
p.Cp = which.min(summary.regfit.bwd$cp)
p.aic = which.min(summary.regfit.bwd$aic)
p.bic = which.min(summary.regfit.bwd$bic)
points(c(p.Cp,p.aic,p.bic),
       c(summary.regfit.bwd$cp[p.Cp],summary.regfit.bwd$aic[p.aic],summary.regfit.bwd$bic[p.bic]))
legend("topright",c("C_p","AIC","BIC"),lty=1,col=1:3,)

#Full search
system.time({regfit.full = regsubsets(lpsa~.-train,data=subset(prostate,train==TRUE))})
summary.regfit.full = summary(regfit.full)
show(summary.regfit.full$rsq)

# R^2 when including each of the covariates
plot(regfit.full,scale="r2") 

#R^2 and R_adj^2 against the no. of cov. in the model
matplot(cbind(summary.regfit.full$rsq,summary.regfit.full$adjr2),type="l") 

#C_p, AIC and BIC against the no. of cov. in the model
summary.regfit.full$aic = summary.regfit.full$bic-log(n)*(c(1:nvmax)+2)+2*(c(1:nvmax)+2)
matplot(cbind(summary.regfit.full$cp,summary.regfit.full$aic,summary.regfit.full$bic),type="l",lty=1)
p.Cp = which.min(summary.regfit.full$cp)
p.aic = which.min(summary.regfit.full$aic)
p.bic = which.min(summary.regfit.full$bic)
points(c(p.Cp,p.aic,p.bic),
       c(summary.regfit.full$cp[p.Cp],summary.regfit.full$aic[p.aic],summary.regfit.full$bic[p.bic]))
legend("topright",c("C_p","AIC","BIC"),lty=1,col=1:3,)

# =============================================
# Hitters data
# =============================================
#
# Summary: 
# These are Major League Baseball data from the 
# 1986 and 1987 seasons. This dataset was taken 
# from the StatLib library. The salary data were 
# originally from Sports Illustrated, April 20, 1987. 
# The 1986 and career statistics were obtained from 
# The 1987 Baseball Encyclopedia Update. 
#
# Data description: 
# The data set consists of the following 20 variables:
# Response variable:
#  Salary: annual salary on opening day in thousands of dollars
# Covariates:
#  AtBat: Number of times at bat 
#  Hits: Number of hits
#  HmRun: Number of home runs
#  Runs: Number of runs 
#  RBI: Number of runs batted
#  Walks: Number of walks
#  Years: Number of years in the major leagues 
#  CAtBat: Number of times at bat during his career 
#  CHits: Number of hits during his career
#  CHmRun: Number of home runs during his career
#  CRuns: Number of runs during his career
#  CRBI: Number of runs batted in during his career
#  CWalks: Number of walks during his career
#  League: A factor with levels A and N indicating player’s league at the end of the first year 
#  Division: A factor with levels E and W indicating player’s division at the end of the first year 
#  PutOuts: Number of put outs
#  Assists: Number of assists
#  Errors: Number of errors
#  NewLeague: A factor with levels A and N indicating player’s league at the beginning of the second year
#
#======================================================

# The data are available in the R package ISLR
library(ISLR)
data("Hitters")
Hitters=na.omit(Hitters) #remove missing values
class(Hitters$League)
class(Hitters$Division)
class(Hitters$NewLeague)

# Fit full model
fit.lm = lm(Salary~.,data=Hitters)
summary(fit.lm)

library(leaps)

### Forward selection
nvmax=19
regfit.fwd = regsubsets(Salary~.,data=Hitters,nvmax=nvmax,method="forward")
summary.regfit.fwd <- summary(regfit.fwd)
show(summary.regfit.fwd)

# R^2 when including each of the covariates
plot(regfit.fwd,scale="r2") 

#R^2 and R_adj^2 against the no. of cov. in the model
matplot(cbind(summary.regfit.fwd$rsq,summary.regfit.fwd$adjr2),type="l") 

#C_p, AIC and BIC against the no. of cov. in the model
summary.regfit.fwd$aic = summary.regfit.fwd$bic-log(n)*(c(1:nvmax)+2)+2*(c(1:nvmax)+2)
matplot(cbind(summary.regfit.fwd$cp,summary.regfit.fwd$aic,summary.regfit.fwd$bic),type="l",lty=1)
p.Cp = which.min(summary.regfit.fwd$cp)
p.aic = which.min(summary.regfit.fwd$aic)
p.bic = which.min(summary.regfit.fwd$bic)
points(c(p.Cp,p.aic,p.bic),
       c(summary.regfit.fwd$cp[p.Cp],summary.regfit.fwd$aic[p.aic],summary.regfit.fwd$bic[p.bic]))
legend("topright",c("C_p","AIC","BIC"),lty=1,col=1:3,)

