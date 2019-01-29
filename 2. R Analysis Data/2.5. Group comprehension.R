library(ggplot2)
?iris
df <- iris

df1 <- subset(df, Species != "setosa")
table(df1$Species)

hist(df1$Sepal.Length)

ggplot(df1, aes(x = Sepal.Length))+
  geom_histogram(fill = "white", col = "black", binwidth = 0.4)+
  facet_grid(Species ~ .)

ggplot(df1, aes(Sepal.Length, fill = Species))+
  geom_density(alpha = 0.6)

ggplot(df1, aes(Species, Sepal.Length))+
  geom_boxplot()

shapiro.test(df1$Sepal.Length)
shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
bartlett.test(Sepal.Length ~ Species, df1)
t.test(Sepal.Length ~ Species, df1)
test1 <- t.test(Sepal.Length ~ Species, df1)
t.test(df1$Sepal.Length, mu = 6.7)

t.test(df1$Petal.Length, df1$Petal.Width, paired = T)
by(iris$Sepal.Length, INDICES = iris$Species, shapiro.test)


#домашнее задание 1
tg <- ToothGrowth
tg$dose <- factor(tg$dose)

t_stat <- t.test(len ~ supp, tg, 
        subset = ((supp == "OJ" & dose == 0.5) | (supp == "VC" & dose == 2)))$statistic


#домашнее задание 2
lek <- read.csv("./Datasets/lekarstva.csv")
head(lek)
str(lek)

t.test(lek$Pressure_before, lek$Pressure_after, paired = T)

#продолжение классной работы
t.test(df1$Petal.Length, df1$Petal.Width, paired = T)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1)+
  stat_summary(fun.y = mean, geom = "point", size = 4)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 1)

wilcox.test(Petal.Length ~ Species, df1)

ggplot(df1, aes(Species, Petal.Length))+
  geom_boxplot()

wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)


#домашнее задание 4
dz <- read.table("./Datasets/dataset_11504_15.txt")
check_ds <- function(x){
  if(bartlett.test(x$V1, x$V2)$p.value < 0.05) {
    print("We do Wilcox test")
    wilcox.test(V1 ~ V2, x)$p.value
  } else {
    print("We do T test")
    t.test(V1 ~ V2, x, var.equal = T)$p.value
  }
}

check_ds(dz)

#домашнее задание 5
dz2 <- read.table("./Datasets/dataset_11504_16.txt")
check_ds_2 <- function(x){
  if(t.test(x)$p.value < 0.05) {
    print(round(c(mean(x$V1), mean(x$V2), t.test(x$V1, x$V2)$p.value), digits = 4))
  } else {
    print("The difference is not significant")
  }
}
check_ds_2(dz2)
