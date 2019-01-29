library(dplyr)
library(caret)
library(AER)
library(ggplot2)
library(sandwich)
library(ivpack)

h <- read.csv("flats_moscow.txt", header = T, sep = '\t', dec = ".")
glimpse(h)


#при прогнозе важно качество прогнозов, а не значимость коэфициентов. Поэтому важно поделить выборку на обучающую и тестовую. Чтобы на тестовой проверить качество прогнозов
train <- createDataPartition(y = h$price, p=0.75, list=F) #create 75% train dataset

htrain <- h[train,]
htest <- h[-train,]

model1 <- lm(data=htrain, log(price) ~ log(totsp) + log(kitsp) + log(livesp))
model2 <- lm(data=htrain, log(price) ~ log(totsp) + brick)

y <- log(htest$price)
yhat1 <- predict(model1, htest)
yhat1
yhat2 <- predict(model2, htest) #create predictions

sum((y-yhat1)^2)
sum((y-yhat2)^2) #select predictions, which have less square of mistakes


#endogeneity
data("CigarettesSW")
h <- CigarettesSW

h2 <- mutate(h, rprice = price/cpi, rincome = income/cpi/population, rtax=tax/cpi)
h3 <- filter(h2, year == "1995")


model0 <- lm(data=h3, log(packs) ~ log(rprice))
summary(model0)

#two-steps-OLS
#step1
st1 <- lm(data=h3, log(price) ~ rtax)
h3$logpricehat <- fitted(st1)
#step2
st2 <- lm(data=h3, log(packs) ~ logpricehat)
summary(st2) # здесь мы видим зафиксированную регрессию: что произойдёт с потреблением, если мы повысим цену на 1%

model_iv <- ivreg(data=h3, log(packs) ~ log(rprice)|rtax) #is the same, that 2 steps behind
summary(model_iv) 

#we can use robust mistakes
coeftest(model_iv, vcov=vcovHC)

# наличие экзогенных регрессоров
model_iv <- ivreg(data=h3, log(packs) ~ log(rprice) + log(rincome)|rtax)
model_iv2 <- ivreg(data=h3, log(packs) ~ log(rprice) + log(rincome)|log(income) + rtax) # так исправляем ошибку
coeftest(model_iv2, vcov. = vcovHC)


# many instrumental variables
h3 <- mutate(h3, rtax2 = (taxs - tax)/cpi)
model_iv3 <- ivreg(data=h3, log(packs) ~ log(rprice) + log(rincome)|log(income) + rtax + rtax2) # так исправляем ошибку
coeftest(model_iv3, vcov. = vcovHC)



#homework
h <- diamonds
set.seed(12345)
train_ind <- createDataPartition(h$price, p=0.8, list=FALSE)
h_train <- h[train_ind,]
h_test <- h[-train_ind,]

tmodel <- lm(data=h_train, log(price) ~ log(carat) + log(depth) + log(table) + clarity)
sum((h_test$price-exp(predict(tmodel, h_test)))^2)/1000000000

library(AER)
data("CollegeDistance")
h <- CollegeDistance

model <- ivreg(data=h, wage ~ region + gender + ethnicity + unemp + education|region + gender + ethnicity + unemp + distance)
summary(model)               
               
               
               
               
               
               
               