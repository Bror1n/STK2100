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
