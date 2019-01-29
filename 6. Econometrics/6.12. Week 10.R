library(spikeslab)
library(ggplot2)
library(reshape2)
library(dplyr)
library(MCMCpack)
library(quantreg)
library(randomForest)
library(rattle)
library(caret)
library(rpart)
library(AER)
library(erer)
library(lmtest)
library(sandwich)
library(forecast)


f <- read.table("./Datasets/flats_moscow.txt", header = T, sep="\t", dec = ".")
glimpse(f)

#quantile regression from quantreg
modelq01 <- rq(data=f, price~totsp, tau=c(0.1, 0.5, 0.9))
summary(modelq01)

base <- qplot(data=f, totsp, price)
base

base_q <- base + stat_smooth(method = "rq", method.args = list(tau = 0.1), se = FALSE) + 
  stat_smooth(method = "rq", method.args = list(tau = 0.9), se = FALSE) + 
  stat_smooth(method = "rq", method.args = list(tau = 0.5), se = FALSE) #create graph with different quantile lines

base_q + aes(color=as.factor(brick))

#random forest from randomForest
in_sample <- createDataPartition(f$price, p=0.75, list=F)
ftr <- f[in_sample, ]
fte <- f[-in_sample, ]


model_lm <- lm(data = ftr, price ~ totsp + kitsp + livesp + brick)
model_rf <- randomForest(data = ftr, price ~ totsp + kitsp + livesp + brick)

y <- fte$price
yhatlm <- predict(model_lm, fte)
yhatrq <- predict(model_rf, fte)

sum((y-yhatlm)^2)
sum((y-yhatrq)^2) # in the randomForest model rss is less

#bayesian method in logit
bad <- data.frame(y = c(0, 0, 1), x = c(1, 2, 3))

model_log <- glm(data=bad, y~x, family = "binomial"(link="logit"))
summary(model_log)

# apriority: beta == normal with dispersion 50^2
model_mcmc_log <- MCMClogit(data=bad, y~x, b0 = 0, B0 = 1 /50^2) #важно, что результаты отличаются от изначальных предположений
summary(model_mcmc_log)

model_mcmc_log <- MCMClogit(data=bad, y~x, b0 = 0, B0 = 1 /10^2)

#pik-plato regression (spike and slab regresison)
h <- mutate(cars,speed=1.61*speed, dist=dist*0.3)
h$junk <- rnorm(nrow(h))

h <- mutate(h, speed2 = speed^2)

model <- lm(data=h, dist~speed+junk)
summary(model)


model_spas <- spikeslab(data=h, dist~speed+junk, n.iter2 = 10000 ) #чем больше выборка, тем точнее вычисления
print(model_spas)

model_spas$summary #bma.scale -- оригинальные переменные и оценки, а алгоритм автоматом убрал мусорную переменную

included_regressors <- melt(model_spas$model)
included_regressors #here we see, which regerssors include in iteration

sum(included_regressors$value==1)/10000
sum(included_regressors$value==2)/10000 #how frequent regressor include in simulation. Also it is chance of meaningful influencing on our regression 



 #homework
qt(0.99, df = 1000)

h <- mtcars
model <- lm(data=h, mpg ~ hp + wt + am)

bptest(model, varformula = ~hp + I(hp^2) + wt + I(wt^2), data=h)
gqtest(model, order.by = ~hp, data=h, fraction = 0.3)


vcovHC(model, type = "HC2")
vcovHC(model, type = "HC3")
vcovHC(model, type = "HC1")
vcovHC(model, type = "HC0")

h <- read.csv("./Datasets/titanic3.csv")
model <- glm(data=h, survived ~ age + I(age^2) + sex + pclass + sibsp, family = binomial(link = "probit"), x = T)
summary(model)
vcov(model)

maBina(model)


set.seed(12)

y<-arima.sim(model = list (ar = c(0.1, 0.6), ma = -0.3), n=100)

x1<-rnorm(100, 15, 5)

x2<-runif(100, 45, 50)


model <- lm(y ~ x1 + x2)
coeftest(model, vcov. = vcovHAC)
bgtest(model, order = 3)

library(zoo)
res <-  zoo(model$residuals)
past_res <- stats::lag(res, 1)
past_res

plot(past_res, res)

set.seed(123)

y<-arima.sim(model = list(ar = c(0.5, 0.1), ma = c(0.3,0.2)), n = 100)
Arima(y, order=c(1,1,1))
Arima(y, order=c(0,1,2))
Arima(y, order=c(1,1,0))
Arima(y, order=c(1,1,2))

Arima(y, order=c(3,0,3), fixed = c(0, NA, NA, 0, NA, NA, NA))

h <- CollegeDistance

set.seed(42)
nums <- createDataPartition(h$wage, p=0.9, list=F)
train <- h[nums,]
test <- h[-nums,]

model <- lm(data=train, wage ~ gender + ethnicity + unemp + education + region)
summary(model)

model2 <- ivreg(data=train, wage ~ gender + ethnicity + unemp + education + region|region + gender + ethnicity + unemp + distance)
summary(model2)

predict(model2, test)



qt(0.95, df = 1000)


2^2*4.5 + 3.4^2*0.05 + 2*2*3.4*(-0.23) + 1200

