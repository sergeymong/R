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
library(grid)
library(vcd)

devtools::install_github('Rapporter/pander')

# графики
h <- diamonds
qplot(data=h, carat, price)
qplot(data=h, log(carat), log(price))

bg <- qplot(data=h, log(carat), log(price))
bg + geom_hex() # график, который показывает количество наблюдений того или иного рода

f <- read.csv("flats_moscow.txt", sep = "\t", header = T, dec = ".")
glimpse(f)
qplot(data = f, totsp, price)

qplot(data = f, log(totsp), log(price))

#vcd хорошо работает для визуализации большого количества данных и факторные переменные
mosaic(data = f, ~walk + brick + floor, shade = T)

# важно для корректной работы графиков факторы преобразовать в факторы
f <- mutate_at(f, vars(walk, brick, floor, code), "factor")

qplot(data = f, log(price), fill = brick, geom = "density", alpha = 0.5) + facet_grid(walk~floor)


# оценки моделей
model0 <- lm(data = f, log(price) ~ log(totsp))
model1 <- lm(data = f, log(price) ~ log(totsp) + brick)
model2 <- lm(data = f, log(price) ~ log(totsp) + brick + brick:log(totsp)) # таким образом указываем произведение переменных
model2_b <- lm(data = f, log(price) ~ log(totsp)*brick) # это то же самое, что и модель 2

summary(model0)
summary(model1)
summary(model2)

mtable(model2, model2_b) #визуальное сравнение двух моделей

plot_model(model2) # если значения пересекают 0, значит коэффициент незначим

mtable(model2)

nw <- data.frame(totsp=c(60, 60), brick=factor(c(1, 0)))

predict(model2, nw)
exp(predict(model2, nw))

exp(predict(model2, nw, interval = "confidence")) # доверительный интервал для среднестатистической квартиры в Москве
exp(predict(model2, nw, interval = "prediction")) # доверительный интервал для конкретной квартиры (предиктивный интервал)


#проверка гипотез об ограничениях, пакет lmtest
waldtest(model0, model1) #гипотеза о верности модели 0 отвергается, Н0 отвергаем
waldtest(model1, model2) #гипотеза о верности модели 0 отвергается, Н0 отвергаем

gg0 <- qplot(data=f, log(totsp), log(price))
gg0 + stat_smooth(method = "lm")
gg0 + stat_smooth(method = "lm") + facet_grid(~walk)
gg0 + aes(col = brick) + stat_smooth(method = "lm") + facet_grid(~walk)


#ловушка дамми, тест рамсея
f$non_brick <- memisc::recode(f$brick, 1 <- 0, 0 <- 1) #создали зеркальную переменную кирпичности дома
glimpse(f)
model_wrong <- lm(data=f, log(price)~log(totsp) + brick + non_brick)
summary(model_wrong) #модель автоматически выкидывает абсолютно зависимые переменные

mtable(model0, model1, model2)

#тест рамсея, lmtest
resettest(model2) #с моделью ещё можно работать, так как гипотеза о том, что все переменные включены -- отвергается


#дз
qf(0.9, df1 = 4, df2 = 35)
((75-25)/2)/(25/(40-5))

pricef <- -80 + 1.9*80


var <- 21.9 + 150^2*0.01 + 2*150*(-0.46)
var
qnorm(0.975)

var <- 21.9 + 150^2*0.01 + 2*150*(-0.46)

str(diamonds)

summary(lm(price ~ carat + y + x, diamonds))

summary(lm(price ~ carat + clarity, diamonds))

model <- lm(price ~ carat, diamonds)
model2 <- lm(price ~ carat + depth, diamonds)
model3 <- lm(price ~ carat + cut, diamonds)
mtable(model, model2, model3)

waldtest(model, model2)
df <- diamonds
resettest(model2)

qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5) + facet_grid(~cut)

qplot(data=df, carat, price, color = clarity) + facet_wrap(~cut)
