# Nuclear Data Analysis
```{R}
# a)
rm(list=ls())

nuclear_data <- read.table(file = "Obligs/nuclear.dat"  , header = TRUE, sep = "\t")
# Instead of transforming the response variable as a separate variable we can instead transform the variable directly in the object
# This makes it so that the lm function is less complex
nuclear_data$log_cost <- log(nuclear_data$cost)

head(nuclear_data)
fit.lm <- lm(log_cost~ date + t1 + t2 + cap + pr + ne + ct + bw + cum.n + pt , data=nuclear_data)
print(names(fit.lm$model))
summary(fit.lm)

conf_int = confint(fit.lm, c("t1","t2","bw"),level = 0.95)
print(conf_int)

```

```{R}

```

```{R}

```