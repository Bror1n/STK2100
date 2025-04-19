# =============================================
# Zip code data
# =============================================
#
# Summary: The data from this example come from the 
# handwritten ZIP codes on envelopes from U.S. postal 
# mail. Each image is a segment from a five digit
# ZIP code, isolating a single digit. The images are 
# 16C16 eight-bit grayscale maps, with each pixel ranging 
# in intensity from 0 to 255.
#
# Data description: 
# The data set consists of the following 257 variables:
# Response variable:
#  cl: true digit (0,1,...,9)
# Covariates:
#  x1-x256: intensity of each of the 256 pixels    
#
#================================================

## Read the data
zip.train <- read.table("https://www.uio.no/studier/emner/matnat/math/STK2100/data/zip.train",
                        col.names=c("cl",paste("x",1:256,sep="")))
zip.test <- read.table("https://www.uio.no/studier/emner/matnat/math/STK2100/data/zip.test",
                       col.names=c("cl",paste("x",1:256,sep="")))
zip = rbind(zip.train,zip.test)
N.tr = nrow(zip.train)
N.te = nrow(zip.test)

#Normalizing covariates
zip[,-1] = scale(zip[,-1])
zip$cl = as.factor(zip$cl)
zip.train = zip[1:N.tr,]
zip.test = zip[N.tr+1:N.te,]
zip.train$cl = as.factor(zip.train$cl)
zip.test$cl = as.factor(zip.test$cl)

## Train a neural network with one hidden layer, here using the squared error loss, as in the book
## This can be replace with the entropy by writing entropy=TRUE in the call to nnet
library(nnet)
decay = seq(0,4.5,length=10)
err.nnet = rep(NA,length(decay))
run.full <- FALSE
if(run.full) # Rund with all lambda values
{
  for(i in 1:length(decay))
  {
    print(i)
    m = 10
    zip.nnet = nnet(cl~.,data=zip[1:N.tr,],size=m,decay=decay[i],MaxNWts=10000,maxit=500)
    pred = predict(zip.nnet,zip[N.tr+1:N.te,],type="class")
    table(zip.test$cl,pred)
    err.nnet[i]=mean(zip.test$cl!=pred)
  }
} else # Just run with the first lambda value
{
  i = 1
  m = 10
  zip.nnet = nnet(cl~.,data=zip[1:N.tr,],size=m,decay=decay[i],MaxNWts=10000,maxit=500)
  pred = predict(zip.nnet,zip[N.tr+1:N.te,],type="class")
  table(zip.test$cl,pred) # the rows are the true labels and the columns the predicted labels 
  err.nnet[i]=mean(zip.test$cl!=pred)
  ## Note: The following call is VERY time-consuming
  ## A pre-run gave the following error rates
  err.nnet[2:10] = 
    c(0.09417040, 0.08719482, 0.08669656, 0.08221226, 
      0.08071749, 0.08719482, 0.08619831, 0.09118087, 0.07922272)
}
  
par(mfrow=c(1,1))
plot(decay,err.nnet)

## Fit a neural network with three hidden layers
library(RSNNS)
zipTargets <- decodeClassLabels(zip[,1])
zipTargets.train = zipTargets[1:N.tr,]
zipTargets.test = zipTargets[N.tr+1:N.te,]
maxit = c(100,200,300,400,500,600,700,800,900,1000)
err.dnet = rep(NA,length(maxit))
run.full <- FALSE

if(run.full) # run with all maximum numbers of iterations 
{
  for(i in 1:length(maxit))
  {
    zip.dnet = mlp(as.matrix(zip.train[,-1]),zipTargets.train, size = c(6,10,6),
                   learnFuncParams=c(0.3),maxit=maxit[i])
    pred.dnet = apply(predict(zip.dnet,zip.test[,-1]),1,which.is.max)-1
    err.dnet[i] = mean(zip.test$cl!=pred.dnet)
  }
} else # run just the first maximum number of iterations
{
  i <- 1
  zip.dnet = mlp(as.matrix(zip.train[,-1]),zipTargets.train, size = c(6,10,6),
                 learnFuncParams=c(0.3),maxit=maxit[i])
  pred.dnet = apply(predict(zip.dnet,zip.test[,-1]),1,which.is.max)-1
  err.dnet[i] = mean(zip.test$cl!=pred.dnet)
  #pre-run
  err.dnet[2:10] = c(0.1335326, 0.1145989, 0.1175884, 0.1275536, 
               0.1190832, 0.1081216, 0.1285501, 0.1170902, 0.1320379)
}
plot(maxit,err.dnet)

