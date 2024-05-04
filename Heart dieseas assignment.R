setwd("C:/Users/rocks/OneDrive/Documents/Heart dieses project")

#################### Heart Disease problem ###################

library(readxl)
> X1666257828_heart <- read_excel("1666257828_heart.xlsx")
> View(X1666257828_heart)

### Question 1-3 #####
Heart = X1666257828_heart
Heart
head(Heart)
tail(Heart)
head(Heart, 10)
dim(Heart)
class(Heart)
summary(Heart)

### Question 4 - checking normality by plotting gg plot graph #####
install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

ggplot(Heart,aes(x = smoking, y = cycling)) + geom_point() +geom_smooth(method = "lm")
ggplot(Heart,aes(x=smoking, y=r)) + geom_point() +geom_smooth(method = "lm")

summary(Heart)
hist(Heart$cycling)
hist(Heart$smoking)
hist(Heart$r)
plot(smoking ~ cycling, data = Heart)
plot(cycling ~ r, data= Heart)

#### Question 5-8 #####
####creating regression model #####
#### relationship between cycling, smoking and heart dieases ####

heart_disease_lm = lm(r ~ cycling + smoking, data = Heart)
summary(heart_disease_lm)

Call:
  lm(formula = r ~ cycling + smoking, data = Heart)

Residuals:
  Min      1Q  Median      3Q     Max 
-2.1789 -0.4463  0.0362  0.4422  1.9331 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 14.984658   0.080137  186.99   <2e-16 ***
  cycling     -0.200133   0.001366 -146.53   <2e-16 ***
  smoking      0.178334   0.003539   50.39   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.654 on 495 degrees of freedom
Multiple R-squared:  0.9796,	Adjusted R-squared:  0.9795 
F-statistic: 1.19e+04 on 2 and 495 DF,  p-value: < 2.2e-16

### The estimated effect of biking on heart disease is -0.2, while the estimated effect of smoking is 0.178.
### This means that for every 1% increase in cycling, there is a correlated 0.2% decrease in the incidence of heart disease
### Meanwhile, for every 1% increase in smoking, there is a 0.178% increase in the rate of heart disease.


## The standard errors for these regression coefficients are very small, 
## and the t statistics are very large (-147 and 50.4, respectively). 
## The p values reflect these small errors and large t statistics. 
## For both parameters, there is almost zero probability that this effect is due to chance.


### Q9-Confidence interval ###

result <- t.test(Heart)
confidence_interval <- result$conf.int
confidence_interval

[1] 20.21516 22.05016
attr(,"conf.level")
[1] 0.95


### Checking for homoscedasticity and diagnostic plot ###
par(mfrow=c(2,2))
plot(heart_disease_lm)
par(mfrow=c(1,1))

heart_graph =  ggplot(heart_disease_lm, aes(x=r, y=smoking))+geom_point()
heart_graph


heart_graph <- heart_graph + geom_smooth(method="lm", col="black")
heart_graph

##### Autocorrelation #####

model <- lm(r~smoking + cycling, data = Heart)

Autocorrelation = acf(model$residuals, type = "correlation")
Autocorrelation
