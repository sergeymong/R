df <- data.frame(swiss)

hist(swiss$Fertility, col = 'red')

fit <- lm(Fertility ~ Examination + Catholic, swiss)
summary(fit)

fit2 <- lm(Fertility ~ Examination*Education, swiss)
summary(fit2)

confint(fit2)

#домашнее задание 1
t <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
head(t)
fill_na <- function(x){
  model <- lm(y ~ x_1 + x_2, x)
  x$y_full <- predict(model, x)
  x$y_full[is.na(x$y) == F] <- x$y[is.na(x$y) == F]
  x
}

#домашнее задание 2
df <- mtcars[,c(6, 1, 3, 5, 4)]
summary(lm(mpg ~ wt + hp, df))

#домашнее задание 3
head(attitude)
summary(lm(rating ~ complaints*critical, attitude))

#продолжение классной работы
swiss$religius <- as.factor(ifelse(swiss$Catholic > 60, 'Lots', 'Few'))

fit3 <- lm(Fertility ~ Examination + religius, swiss)
summary(fit3)

fit4 <- lm(Fertility ~ Examination*religius, swiss)
summary(fit4)
View(mtcars)

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(swiss, aes(x = Examination, y = Fertility, col = religius)) + 
  geom_point()+
  geom_smooth(method = 'lm')

fit5 <- lm(Fertility ~ religius*Examination*Infant.Mortality, swiss)
summary(fit5)

#домашнее задание 4
mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))

summary(lm(mpg ~ wt*am, mtcars))

my_plot <- ggplot(mtcars, aes(y = mtcars$mpg, x = wt, col = mtcars$am)) + 
  geom_smooth(method = 'lm')
