library(HSAUR)
library(dplyr)
library(psych)
library(lmtest)
library(glmnet)
library(ggplot2)
library(car)

h <- cars
qplot(data=h, speed, dist)

model <- lm(dist ~ speed, h)
summary(model)

h <- mutate(speed2 = speed^2, speed3 = speed^3, h)
modelh <- lm(dist ~ speed + speed2 + speed3, h)
summary(modelh)

vif(modelh)

x0 <- model.matrix(data=h, dist ~ 0+ speed + speed2 + speed3) #добавляя 0, мы убираем интерсепт при расчёте
head(x0)

cor(x0)

nd <- data.frame(speed = 10, speed2 = 100, speed3 = 1000)
predict(model, newdata = nd, interval = "prediction")
predict(modelh, newdata = nd, interval = "prediction") # при предсказании доверительные интервалы менются слабо

confint(model)
confint(modelh) # доверительные интервалы для модели


#LASSO и Ridge регрессии
#для начала необходимо отдельно получить значения ЗП и матрицу регрессоров
y <- h$dist
x0 <- model.matrix(data=h, dist ~ 0+ speed + speed2 + speed3)

#LASSO
lambdas <- seq(50, 0.1, length = 30) # важно указывать лямбды в порядке убывания
m_lasso <- glmnet(x0, y, alpha=1, lambda = lambdas) # альфа -- каккой тип регуляризации, 1 -- лассо

plot(m_lasso, xvar = "lambda", label = T) #чем меньше логарифм лямбда, тем ближе к МНК и разницы нет.
plot(m_lasso, xvar = "dev", label = T) # по оси x доля объясн. дисперсии
plot(m_lasso, xvar = "norm", label = T) # по оси x доля объясн. дисперсии

coef(m_lasso, s=c(0.1, 1))

#Ridge
m_ridge <- glmnet(x0, y, alpha=0, lambda = lambdas)

# для выбора лучше лямбда используется метод кросс-валидации. Выбираем ту лямбда, при котором сумма квадратов ошибок минимальна
cv <- cv.glmnet(x0, y, alpha=1)
plot(cv) # черты на графике -- оптимальные лямбда

cv$lambda.min # минимизирует сумму квадратов ошибок
cv$lambda.1se # при нём коэффициенты ближе к нулю -- максимум лямбда

coef(cv, s = "lambda.min")
coef(cv, s = "lambda.1se")

#РСА или метод главных компонент
h <- heptathlon
h <- select(h, -score)

#так как у нас есть разные единицы измерения, надо нормализовать их
cor(h)
h.pca <- prcomp(h, scale=T)
pcal1 <- h.pca$x[,1]
v1 <- h.pca$rotation[,1] # веса, с которыми старые переменные входят в новую переменную
v1

summary(h.pca)
plot(h.pca)

biplot(h.pca, xlim=c(-1, 1)) #иногда МГК позволяет кластеризиовать данные, в нашем случае мы получили 2 кластера -- Launa и остальные спортсмены:)

data<-na.omit(data) # позволяет убрать все NA из данных

#дз
age <- seq(30, 65)
pension <- 65-age

# 0  3
# 1  2
# 2  1
# 3  0

qplot(Ozone, Temp, data=airquality)

fit <- lm(Ozone ~ 0 + Solar.R + Wind + Temp, airquality)
vif(fit)

d <- na.omit(airquality)
y <- d$Ozone
mat <- model.matrix(fit)
lam <- seq(50,0.1,length=30)

glm <- glmnet(mat, y, alpha = 0, lambda = lam)
coef(glm, s=2) #смотрим на коэффициенты при лямбда = 2


comps <- prcomp(mat, scale = T)
qplot(comps$x[,1], comps$x[,3])

