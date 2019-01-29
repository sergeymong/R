library(sandwich) #vcovH
library(lmtest)
library(car)
library(dplyr)
library(broom) #манипуляция с данными
library(ggplot2)
install.packages("Ecdat")
library(Ecdat)

h <- read.table("./Datasets/flats_moscow.txt", header = T)

qplot(totsp, price, data=h) #то, как выглядит гетероскедастичность

model <- lm(price~totsp, h)
summary(model)

coeftest(model)
confint(model)

vcov(model)

h <- augment(model, h) #добавили ключевые показатели модели в таблицу
glimpse(h)
qplot(data=h, totsp, abs(.resid)) #видим, что остатки тоже не гомоскедастичны

vcovHC(model)
vcovHC(model, type="HC2")

coeftest(model, vcov. = vcovHC(model)) #подменяем ошибки на исправленные

#строим доверительные интервалы
conftable <- coeftest(model, vcov. = vcovHC(model))
ci <- data.frame(estimate=conftable[,1], se_hc=conftable[,2])
ci <- mutate(ci, left_ci=estimate-1.96*se_hc, right_ci=estimate+1.96*se_hc) #получили более широкие доверительные интервалы, в сравнении со стандартной моделью


#тесты на гетероскедастичность
bptest(model) #тест бройша-пагана, или тест Уайта, видим гетероскедастичность
bptest(model, data = h, varformula = ~totsp + I(totsp^2)) #классический тест Уайта

#тест голдфельда
gqtest(model, order.by = ~totsp, data=h, fraction = 0.2)


#помимо робастных ошибок, можно применять логарифмирование переменных
qplot(data=h, log(totsp), log(price))

model2 <- lm(data=h, log(price)~log(totsp))
gqtest(model2, order.by = ~totsp, data=h, fraction = 0.2)
gqtest(model, order.by = ~totsp, data=h, fraction = 0.2)

#1-120/TSS = 0.4; 120/TSS = 0.6; TSS = 120/0.6; TSS = 200

ChickWeight %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean(weight))

summary(lm(weight ~ Time + Diet, ChickWeight))

summary(lm(price ~ carat + table + x + y + z + depth, diamonds))

md <- lm(price ~ carat + table + x + y + depth, diamonds)
confint(md, level = 0.9)

BudgetFood

model <- lm(wfood ~ totexp + size, BudgetFood)
summary(model)
nd <- data.frame(size=5, totexp=700000)
predict(model, newdata = nd, interval = "confidence")


resettest(model)


h <- na.omit(BudgetFood)
lm1 <- lm(wfood ~ totexp + size, h)
lm2 <- lm(wfood ~ totexp + size + sex, h)

anova(lm1, lm2)

f
vif(lm(mpg ~ disp + hp + wt, mtcars))

mtcars

sqrt(sum(prcomp(mtcars[,c(3, 4, 6)], scale=T)$x[,1]^2))

c <- prcomp(mtcars[,c(3, 4, 6)], scale=T)$x

c
summary(lm(mpg ~ c[,1] + c[,2], mtcars))$r.squared - summary(lm(mpg ~ c[,1] + c[,2] + c[,3], mtcars))$r.squared

nd <- data.frame(size=4,totexp=700000)

predict(model, newdata=nd, interval="prediction", level=0.9)



model_r <- lm(data=h, wfood~totexp+size)

model_ur <- lm(data=h, wfood~totexp*sex+size*sex)

waldtest(model_r,model_ur)
