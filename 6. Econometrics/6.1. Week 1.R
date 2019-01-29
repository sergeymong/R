library(psych)
library(dplyr)
library(ggplot2)
library(GGally)

d <- cars
glimpse(d)
summary(d)
tail(d, 10)

d2 <- mutate(d, speed = 1.67*speed, dist = 0.3*dist, ratio = dist/speed)
d2

qplot(data=d2, dist)

fit <- lm(dist ~ speed, d2)
fit

beta_hat <- coef(fit)
eps_hat <- residuals(fit)
eps_hat
y_hat <- fitted(fit)
y_hat
y <- d2$dist
RSS <- deviance(fit)
TSS <- sum((y-mean(y))^2)
ESS <- TSS - RSS
R2 <- ESS/TSS
R2 <- cor(y,y_hat)^2

X <- model.matrix(fit)

nd <- data.frame(speed=c(40, 60))
nd$dist <- predict(fit, nd)


t <- swiss
describe(t)        


ggpairs(t)
model <- lm(Fertility~Agriculture+Education+Catholic, t)
coef(model)


-1*0.3+0*0.2+1*0.5

cor(c(2, 2, 2, 2, 2), c(2.1, 2.5, 1.9, 1.5, 2))^2

data(sleep)
sleep[7,1]
glimpse(sleep)
mean(sleep$extra)^2
max(sleep$extra) * min(sleep$extra)

var(sleep[c(10:20),]$extra)

summary(lm(mpg~hp+wt+am, mtcars))$r.squared
summary(lm(mpg~cyl+hp+wt, mtcars))$r.squared
summary(lm(mpg~cyl+wt+am, mtcars))$r.squared
summary(lm(mpg~hp+cyl+am, mtcars))$r.squared






