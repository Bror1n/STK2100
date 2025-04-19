# =============================================
# Bone mineral density data
# =============================================
#
# Summary: 
# These are records of bone mineral density (BMD) in 
# adolescents, and how it changes depending on age 
# and gender
# 
#
# Data description: 
# The data set consists of the following 3 variables
# Response variable:
#  spnbmd: relative change in spinal BMD over two consecutive visits
# Covariates:
#  age: age in years
#  gender (male/femal)
#
#======================================================

## Read the data
bone = read.table("http://www.uio.no/studier/emner/matnat/math/STK2100/data/bone.data",header=TRUE)

## Plot the data
col.vec <- rep(2,nrow(bone))
col.vec[bone$gender=="female"] <- 4
plot(bone$age,bone$spnbmd,col=col.vec,pch=16,xlab="age",ylab="relative change in bone mineral density")
legend("topright",c("male","female"),pch=rep(16,2),col=c(2,4))

## Fit separate regression splines with df=12 to the two genders
library(splines)
d.pred = data.frame(age = sort(bone$age))
fit.m = lm(spnbmd~ns(age,df=12),data=bone,subset=(gender=="male"))
pred.m = predict(fit.m,d.pred)
summary(fit.m)
fit.f = lm(spnbmd~ns(age,df=12),data=bone,subset=(gender=="female"))
pred.f = predict(fit.f,d.pred)
summary(fit.f)

lines(d.pred$age,pred.m,type="l",col=2,lwd=3)
lines(d.pred$age,pred.f,type="l",col=4,lwd=3)

termplot(fit.m,se=TRUE)
termplot(fit.f,se=TRUE)

## Select lambda to obtain df=12
bone.m = bone[bone$gender=="male",]
fit.m.s.df = smooth.spline(bone.m$age,bone.m$spnbmd,df=12)
fit.m.s.df
bone.f = bone[bone$gender=="female",]
fit.f.s.df = smooth.spline(bone.f$age,bone.f$spnbmd,df=12)
fit.f.s.df

## Select lambda using (generalised) cross-validation
fit.m.s.cv = smooth.spline(bone.m$age,bone.m$spnbmd,cv=FALSE)
fit.m.s.cv
pred.m.cv = predict(fit.m.s.cv,d.pred)

fit.f.s.cv = smooth.spline(bone.f$age,bone.f$spnbmd,cv=FALSE)
fit.f.s.cv
pred.f.cv = predict(fit.f.s.cv,d.pred)

plot(d.pred$age,pred.m,type="l",col=2,lwd=3)
lines(unlist(pred.m.cv$x),unlist(pred.m.cv$y),col=3,lwd=3)

plot(d.pred$age,pred.f,type="l",col=4,lwd=3)
lines(unlist(pred.f.cv$x),unlist(pred.f.cv$y),col=3,lwd=3)

# =============================================
# South African heart disease data
# =============================================
#
# Summary: 
# The data in Figure 4.12 are a subset of the Coronary Risk-Factor Study (CORIS) 
# baseline survey, carried out in three rural areas of the Western Cape, South 
# Africa (Rousseauw et al., 1983). The aim of the study was to establish the 
# intensity of ischemic heart disease risk factors in that high-incidence region. 
# The data represent white males between 15 and 64, and the response variable is 
# the presence or absence of myocardial infarction (MI) at the time of the survey 
# (the overall prevalence of MI was 5.1% in this region). There are 160 cases in 
# our data set, and a sample of 302 controls. These data are described in more 
# detail in Hastie and Tibshirani (1987).
# 
#
# Data description: 
# The data set consists of the following 10 variables
# Response variable:
#  chd: has coronary heart disease (0=No/1=Yes)
# Covariates:
#  sbp: systolic blood pressure
#  tobacco: lifetime consumption of tobacco in kg
#  ldl: LDL cholesterol level
#  adiposity: body fat
#  famhist: family history of CHD (Present/Absent)
#  typea: type A behaviour score
#  obesity: body mass index
#  alcohol: alcohol consumption
#  age: age in years
#
#======================================================

## Read the data
saheart = read.table("http://www.uio.no/studier/emner/matnat/math/STK2100/data/saheart.data",header=TRUE,sep=",")

## Fit an ordinary logistic regression model
fit.logit  <-  glm(chd~sbp+tobacco+ldl+famhist+obesity+alcohol+age,family=binomial,data=saheart)
summary(fit.logit)
# Run a test for removing each of the covariates, while keeping the rest
anova(fit.logit,test="Chisq")

# Remove the least sigificant covariate
fit.logit.2 = glm(chd~sbp+tobacco+ldl+famhist+obesity+age,family=binomial,data=saheart)
summary(fit.logit.2)
anova(fit.logit.2,test="Chisq")
AIC(fit.logit,fit.logit.2)

# Remove the least sigificant covariate
fit.logit.3 = glm(chd~sbp+tobacco+ldl+famhist+age,family=binomial,data=saheart)
summary(fit.logit.3)
anova(fit.logit.3,test="Chisq")
AIC(fit.logit,fit.logit.2,fit.logit.3)

## Fit a logistic regression with natural cubic splines
library(splines)
fit.ns = glm(chd~ns(sbp,df=4)+ns(tobacco,df=4)+ns(ldl,df=4)+ns(obesity,df=4)+ns(alcohol,df=4)+ns(age,df=4)+famhist,family=binomial,data=saheart)
summary(fit.ns)
termplot(fit.ns,se=TRUE,terms=c("ns(sbp, df = 4)","ns(tobacco, df = 4)","ns(ldl, df = 4)","ns(obesity, df = 4)","ns(alcohol, df = 4)","ns(age, df = 4)"))
c(logLik(fit.logit),logLik(fit.ns))
AIC(fit.logit,fit.ns)
anova(fit.ns,test="Chisq")

# Remove the least significant covariate
fit.ns.2 = glm(chd~ns(sbp,df=4)+ns(tobacco,df=4)+ns(ldl,df=4)+ns(obesity,df=4)+ns(age,df=4)+famhist,family=binomial,data=saheart)
summary(fit.ns.2)
AIC(fit.logit.2,fit.ns.2)
termplot(fit.ns.2,se=TRUE,terms=c("ns(sbp, df = 4)","ns(tobacco, df = 4)","ns(ldl, df = 4)","ns(obesity, df = 4)","ns(age, df = 4)"))
anova(fit.ns.2,test="Chisq")

# Remove the least significant covariate
fit.ns.3 = glm(chd~ns(sbp,df=4)+ns(tobacco,df=4)+ns(ldl,df=4)+ns(age,df=4)+famhist,family=binomial,data=saheart)
AIC(fit.logit.3,fit.ns.3)
termplot(fit.ns.3,se=TRUE,terms=c("ns(sbp, df = 4)","ns(tobacco, df = 4)","ns(ldl, df = 4)","ns(age, df = 4)"))
anova(fit.ns.3,test="Chisq")

# The effect of ldl seems close to linear
fit.ns.4 = glm(chd~ns(sbp,df=4)+ns(tobacco,df=4)+ldl+ns(age,df=4)+famhist,family=binomial,data=saheart)
AIC(fit.logit.3,fit.ns.3,fit.ns.4)
termplot(fit.ns.4,se=TRUE,terms=c("ns(sbp, df = 4)","ns(tobacco, df = 4)","ns(age, df = 4)"))

# The effect of tobacco seems close to linear
fit.ns.5 = glm(chd~ns(sbp,df=4)+tobacco+ldl+ns(age,df=4)+famhist,family=binomial,data=saheart)
termplot(fit.ns.5,se=TRUE,terms=c("ns(sbp, df = 4)","ns(age, df = 4)"))
AIC(fit.logit.3,fit.ns.3,fit.ns.4,fit.ns.5)

