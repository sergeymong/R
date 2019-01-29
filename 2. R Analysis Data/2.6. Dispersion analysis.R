mydata <- read.csv("./Datasets/shops.csv")

ggplot(mydata, aes(x = store, y = price, fill = origin))+
  geom_boxplot()

fit <- aov(price ~ origin, mydata) # однофакторный ANOVA
summary(fit)

fit1 <- aov(price ~ origin + store, mydata) # двухфакторный ANOVA
summary(fit1)

model.tables(fit1, "means")

pd = position_dodge(0.1)
ggplot(mydata, aes(x = store, y = price, color = origin, group = origin))+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, lwd = 0.8, position = pd)+
  stat_summary(fun.data = mean_cl_boot, geom = "line", size = 1.5, position = pd)+
  stat_summary(fun.data = mean_cl_boot, geom = "point", size = 5, position = pd, pch = 15)+
  theme_bw()
  
fit3 <- aov(price ~ origin * store, mydata)
summary(fit3)


# домашнее задание 1. Узнаём влияние на урожайность применения азота и фосфатов
datah <- npk
fith <- aov(yield ~ N * P, datah)
summary(fith)

# домашнее задание 2. Узнаём влияние на урожайность применения всех удобрений
fith1 <- aov(yield ~ N + P + K, datah)
summary(fith1)

# продолжение классной работы
ggplot(mydata, aes(x = food, y = price))+
  geom_boxplot()

fit5 <- aov(price ~ food, mydata)
summary(fit5)
TukeyHSD(fit5)

# домашнее задание 3
datah2 <- iris

fith2 <- aov(Sepal.Width ~ Species, datah2)
TukeyHSD(fith2)

# продолжение классной работы
mydata2 <- read.csv("./Datasets/therapy_data.csv")
mydata2$subject <- as.factor(mydata2$subject)

fita <- aov(well_being ~ therapy, mydata2)
summary(fita)

fitb <- aov(well_being ~ therapy + Error(subject/therapy), mydata2)
summary(fitb)

fitc <- aov(well_being ~ therapy*price, mydata2)
summary(fitc)

ggplot(mydata2, aes(x = price, y = well_being))+
  geom_boxplot()

fit2b <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)), mydata2)
summary(fit2b)

ggplot(mydata2, aes(x = price, y = well_being))+
  geom_boxplot()+
  facet_grid(~subject) # бьём график по испытуемым

fitd <- aov(well_being ~ therapy*price*sex, mydata2)
summary(fitd)
fitd2 <- aov(well_being ~ therapy*price*sex + Error(subject/(therapy*price)), mydata2)
summary(fitd2)

ggplot(mydata2, aes(x = price, y = well_being, fill = sex))+
  geom_boxplot()+
  facet_grid(~subject) # бьём график по испытуемым

# домашнее задание 4
mydata3 <- read.csv("./Datasets/Pillulkin.csv")
str(mydata3)
mydata3$patient <- as.factor(mydata3$patient)

fith3 <- aov(temperature ~ pill + Error(patient/pill), mydata3)
summary(fith3)

fith3b <- aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), mydata3)
summary(fith3b)

#домашнее задание 5

obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, color = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))
obj
