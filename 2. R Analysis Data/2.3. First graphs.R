library(ggplot2)

df <- mtcars
df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$am, labels = c("Auto", "Manual"))

hist(df$mpg, breaks = 20, xlab = "MPG")
boxplot(mpg ~ am, df, ylab = "MPG")
plot(mpg ~ am, df)

ggplot(df, aes(x = mpg))+
  geom_histogram(fill = "white", col = "black", binwidth = 1)

ggplot(df, aes(x = mpg, fill = am))+
  geom_dotplot()

ggplot(df, aes(x = mpg))+
  geom_density(fill = "red")

ggplot(df, aes(x = mpg, fill = am))+
  geom_density(alpha = 0.5)

ggplot(df, aes(x = am, y = hp, col = vs))+
  geom_boxplot()

myplot <- ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec))+
  geom_point()


mean_mpg <- mean(df$mpg)
descr_df <- describe(df[, -c(8:9)])

boxplot <- ggplot(df, aes(x = factor(am), y = disp))+
  geom_boxplot()+
  xlab("Transmission")+
  ylab("Displacement (cu.in.)")+
  ggtitle("Box - plot")

write.csv(df, "df.csv")
write.csv(descr_df, "descr_df.csv")


#Домашнее задание 1
head(airquality)
ggplot(airquality, aes(x = as.factor(Month), y = Ozone))+ #важно, что обычные количественные надо преобразовывать в фактор
  geom_boxplot()

#Домашнее задание 2
plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, colour = hp, size = hp))+
  geom_point()
plot1

#Домашнее задание 3
head(iris)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species, size = Petal.Length))+
  geom_point(alpha = 0.6)
