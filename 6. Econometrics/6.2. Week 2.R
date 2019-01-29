#random values
library(memisc)
library(dplyr)
library(psych)
library(lmtest)
library(sjPlot)
library(sgof)
library(ggplot2)
library(foreign)
library(car)
library(hexbin)
library(rlms)


#как генерировать случайные величины
# Z1, ..., Z1000 ~ N(5, 9)
z <- rnorm(1000, mean = 5, sd = 3)
qplot(z)

#построение функции плотности
x <- seq(-10, 15, by = 0.5)
y <- dnorm(x, mean = 5, sd = 3) # построение функции плотности, все начинаются с d -- density

qplot(x, y)
qplot(x, y, geom = "line")

#расчёт вероятности, например, Z<3
#P(Z<3)=F(3)

pnorm(3, mean = 5, sd = 3) #функция распределения

#P(Z in [4;9])
#P(Z<9)-P(Z<4)
pnorm(9, mean = 5, sd = 3) - pnorm(4, mean = 5, sd = 3)

#квантиль распределения: такое число а, чтобы вероятность Z меньше a была равна чему-то от 0 до 1 -- вероятность
#P(Z<a)=0.7 a? 

qnorm(0.7, mean = 5, sd = 3)


# chisq, t, f
# rchisq, dchisq, pchisq, qchisq
# rt, dt, pt, qt
# rf, df, pf, qf


# множественная регрессия
h <- swiss

model <- lm(Fertility ~ Catholic + Agriculture + Examination + Infant.Mortality, h)
summary(model)

confint(model)
plot_model(model, show.intercept = F, show.data = T, show.values = T) # удобный график для оценки того, какие коэффициенты значимы, а какие нет

# проверка гипотезы b_Cath=b_Agri

model_aux <- lm(Fertility ~ Catholic+I(Catholic+Agriculture) + Examination, h) # важно, что Catholic стоит первым, если коэффициенты больше доверительного интервала, 
#что гипотеза равенства католик и агрикалча не отвергается (коэффициенты равны)
summary(model_aux)

#это более удобный способ той же проверки гипотез, нужен пакет car
linearHypothesis(model, "Catholic-Agriculture=0")

#стандартизрованные коэффициенты
h_stand <- mutate_all(h, "scale")

model_st <- lm(Fertility ~ Catholic + Agriculture + Examination, h_stand)
summary(model_st)

plot_model(model_st, show.data = T, show.values = T)

#искуственный эксперимент

D <- matrix(nrow = 100, rnorm(100*41, mean = 0, sd = 1))
df <- data.frame(D)


model_null <- lm(X1 ~ ., df)
summary(model_null)

# сравнение нескольких моделей
model2 <- lm(Fertility ~ Catholic + Agriculture, h)

compar_ln <- mtable(model, model2)
compar_ln

# сохранение результатов работы -- компактный внутренний формат R, но он нечитабельный, лучше сохранять в csv или txt
stuff <- list(data=h, model=model2)
saveRDS(file = "mydata.RDS", stuff)

mylist <- readRDS("mydata.RDS")
mylist

#загрузка RLMS

h <- read.rlms("r21i_os26c.sav")
saveRDS(h, "r21i_os26c.sav")
h <- readRDS("r21i_os26c.sav")

h2 <- select(h, qm1, qm2, qh6, qh5)
describe(h2)
summary(h2)

h3 <- rename(h2, weight=qm1, height=qm2, sex=qh5, b_year=qh6)
h3 <- mutate(h3, age=2012-b_year)
describe(h3)

values_by_sex <- table(h3$sex)
names(values_by_sex) <- c('Man', 'Woman')
(values_by_sex[[2]]/values_by_sex[[1]] - 1) * 100

qplot(data=h3, weight, height)


cor.test(h3$weight, h3$height)
cor.test(h3[h3$weight < 50, ]$weight, h3[h3$weight < 50, ]$height)
cor.test(h3[h3$weight > 50, ]$weight, h3[h3$weight > 50, ]$height)
mean(h3[h3$weight < 50, ]$age, na.rm = T)
mean(h3[h3$weight > 50, ]$age, na.rm = T)

sum(h3$age < 18) / nrow(h3) * 100


h4 <- filter(h3, sex==1)

qplot(data=h4, rost, ves)
qplot(data=h4, ves)


#домашнее задание
pnorm(3, mean = 5, sd = 3)
vcov(c(7.3, 5, -6))

qt(0.95, df = 53936)

pchisq(9, df=10)


sum(diamonds$cut == "Premium")
summary(lm(data=diamonds, price ~ carat + 1))
summary(glm(data=diamonds, price ~ carat + table))
glm(data=diamonds, price ~ carat + clarity)

summary(lm(price ~ carat + x + y , diamonds))


anova(lm(price ~ carat + y, diamonds))

round(pt(4, df = 2), 2)
