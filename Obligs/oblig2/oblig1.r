# Oblig 2 STK2100 Bror Johannes Tidemand Ruud

# Problem 1
wage <- read.csv("https://www.uio.no/studier/emner/matnat/math/STK2100/v25/oblig/wage.csv",header=TRUE)
head(wage)
#summary(wage) #We can see that the qualitative variables are not properly understood by R

# Make R treat all qualitative factors as qualitative
wage$maritl <- as.factor(wage$maritl)
wage$race <- as.factor(wage$race)
wage$education <- as.factor(wage$education)
wage$jobclass <- as.factor(wage$jobclass)
wage$health <- as.factor(wage$health)
wage$health_ins <- as.factor(wage$health_ins)

summary(wage)

#a) (i)
length(wage$maritl) #length of dataset is 3000

# I was a bit unsure on how to do statified sampling on a data set with so many qualitative varibales
# so i instead do a random sample, and then afterward check if all all the qualitative varibales are 
# present and repeat this until they are

# Got some help from chatgpt from this part especially using function(v)
set.seed(420)

valid_split <- FALSE
while(!valid_split) {
    train_index <- sample(1:nrow(wage),size = floor(0.7 * nrow(wage)))
    train <- wage[train_index, ]
    test <- wage[-train_index, ]

    qual_var <- c("maritl","race","education","jobclass","health","health_ins")

    check_all <- sapply(qual_var, function(v) {
        table_train <- table(train[[v]])
        table_test <- table(test[[v]])

        all(table_train >= 154) && all(table_test >= 66) #returs true if there are more than 7 in train and 3 in test
        # that specific qualiatative varibale
    })
    #print("iteration")
    #print(check_all) # We can look on how many of the qualitative predictors where distributed properly
    valid_split <- all(check_all) # returns true if check all only consits of true values ie, we have enough of the different qualitative variables
}

# Tried tweaking the values to maximize the spliting of qualitative values to be distributed as equally as possbible following a 
# 70/30 split. I now realize this program was overkill but i thought there was a higher percentage of the qualitative variables not being present # (ii)
fit.lm <- lm(wage ~ .,data = train)
summary(fit.lm)

# It seems that every covariate other than raceOther(0.754) is significant to a varying degree. Where raceWhite(0.04) follows as the next least
# significant predictor. While being umarried(3.8e-16) and not having health isurance(2e-16) seems to be the most statistically significant on 
# predicting wage

# (iii)
pred.lm <- predict(fit.lm, newdata=test)
mse.lm <- mean((test$wage - pred.lm)^2)
print(mse.lm)


#b) (i)
library(gam)
fit.gam <- gam(wage ~ s(year, df=4) + s(age, df=4) + maritl + race + education + jobclass + health + health_ins , data = train)
summary(fit.gam)

# (ii)
# Fitting splines only work for continious predictors as the natural cubic splines consists of multiple polynomials 
# which would not make much to fit to non continious predictors.

# (iii)
# Well we can look at this question from 2 perspective 
# Firstly we know that both predictors in their nature that they both increase the same from year to year. This however does not mean
# that they have a linear relationship with wage. For example we less both before coming of working age, and after retiring. So this
# can not be modelled linearly. The same can be said for the year, where it does not necessarly follow a linear realtionship. For example
# during an economic recession where wages dicrease.

# Secondly we can see that using a splin on the predictor year is not very significant(0.432 F statistic) so it does not matter if we model it as a spline.
# and it is therefore usually beneficial to keep the simpler model

# In conclusion we can model year linearly as it does not matter much, despite it not necessarily having a linear relationship with wage.

# (iv)
# From the summary of fit.gam it seems that all the covariates are significant and where jobless seems to be the least significant with a
# p value of 0.00147. It therefore seems beneficial to keep all the predictors.

pred.gam <- predict(fit.gam, newdata=test)
mse.gam <- mean((test$wage-pred.gam)^2)
print("The mse prediciton error for the gam is")
print(mse.gam)
print("The mse prediction error for the linear model is")
print(mse.lm)

# The MSE for the two models are about the same with the MSE for the GAM being slighly better. Since the difference is small i would prefer
# using the simpler model. It would be beneficial to do a more robust test of the model to get a better estimate of the different. 
# One way we could do this is using cross validation

#c (i)
library(tree)
fit.tree <- tree(wage ~ ., data=train)
plot(fit.tree)
text(fit.tree,all=TRUE)

# We can see that the splits are mainly in education. The first split is in Some college or less, and collage grad and more.
# The left tree then splits in whether they have health insurance or not. And the right tree splits on college grad and advanced degree. 
# Which in turn has a left tree that splits further in whether they have health insurance or not. 

#We can see from this tree that people with higher degrees and health insurance are more likely to have a higher wage.
# This is similar to our previous models where health insurance and education were among the most significant.

# (ii)
cv.tree.result <- cv.tree(fit.tree, K= 10)

plot(cv.tree.result$size,cv.tree.result$dev, type="b",xlab="Tree size",ylab="Deviance",main="Cross Validation")

#We can see from that plot that the best tree has 5 terminal nodes, it even looks like we should add more as deviance doesnt seem to stabilize
# In other words we keep the original tree
pruned.tree <- prune.tree(fit.tree,best=5)
plot(pruned.tree)
text(pruned.tree,all=TRUE)

# We can see from the plot that they are the same tree, which shows there was no benefit of pruning the tree

# (iii)
pred.tree <- predict(fit.tree,newdata=test)
mse.tree <- mean((test$wage - pred.tree)^2)


print("The mse prediciton error for the gam is")
print(mse.gam)
print("The mse prediction error for the linear model is")
print(mse.lm)
print("The mse prediction error for the regression tree is")
print(mse.tree)

# We can see that the prediction error for the regression tree is the highest this far. I would therefore choose the simplest model
# the linear model even though it has a bit more error than our additive model since it is small error difference and the linear model
# is by far the simplest






