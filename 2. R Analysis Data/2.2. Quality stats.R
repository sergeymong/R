library(psych)

df <- mtcars

range(df$mpg)

#возвращает список базовых описательных статистик
summary(df$hp[df$cyl != 3 & df$am == "Auto"])

#если делаем с помощью стандартного синтаксиса, а не формул, то лучше ещё переименовывать колонки
mean_hp <- aggregate(df$hp, by = list(df$vs, df$am), mean)
colnames(mean_hp) <- c("VS", "Gear", "Mean HP")

#функции и уравнения в aggregate
descriptions_stat <- aggregate(cbind(hp,disp) ~ am, mtcars, sd)
descriptions_stat1 <- aggregate(df[, c(4, 3)], by = list(df$am), sd)
descriptions_stat == descriptions_stat1

#использование библиотеки psych
descr <- describe(df[,-c(8:9)]) #для каждой из переменных рассчитывает разные описательные статистики
describeBy(df[,-c(8,9)], df$vs, mat = T, digits = 1, fast = T) #то же, что и сверху, только по группам

#работа с пропущенными значениями
sum(is.na(df))
df$mpg[1:10] <- NA
mean(df$mpg)
mean(df$mpg, na.rm = T)
aggregate(mpg ~ am, df, mean)

#Домашнее задание 1. Важно!!! условие airquality$Month == c(7:9) выдаёт неправильно количество данных, фактически, поделённное на 3
airq <- subset(airquality, airquality$Month %in% c(7:9)) 
result <- aggregate(Ozone ~ Month, airq, length)
result

#Домашнее задание 2.
airq <- airquality
describeBy(airq[,c(1:4)], airq$Month)

#Домашнее задание 3.
describeBy(iris[,-c(5)], iris$Species)

#Домашнее задание 4.
my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA
#вар1
fixed_vector <- replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))
#вар2
fixed_vector1 <- my_vector
fixed_vector1[is.na(fixed_vector)] <- mean(fixed_vector1, na.rm = T)


