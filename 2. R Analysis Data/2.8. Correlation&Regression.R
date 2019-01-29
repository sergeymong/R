df <- mtcars
cor.test(mtcars$mpg, mtcars$hp)
t <- cor.test(~ mpg + hp, df)


plot(mtcars$mpg, mtcars$hp)

df_numeric <- df[, c(1,3:7)]

pairs(df_numeric)

cor(df_numeric)

#домашнее задание 1
corr.calc <- function(x){
  co <- cor.test(x[,1], x[,2])
  return(c(co$estimate, co$p.value))
}

corr.calc(mtcars[, c(1,5)])

#домашнее задание 2
step6 <-  read.table("./Datasets/step6.csv",  header=TRUE, sep=',' )
#вариант 1 -- мой
library(psych)
filtered.cor <- function(x){
  num_cols <- sapply(x, is.numeric)
  cor <- corr.test(x[, num_cols])
  diag(cor$r) <- 0
  max <- max(as.vector(cor$r)[-1])
  min <- min(as.vector(cor$r)[-1])
  if (abs(max) >= abs(min)) max else min
}
#вариант 2
filtered.cor1 <- function(x){    
  num_var <- sapply(x, function(x) is.numeric(x))    
  cor_mat <- cor(x[, num_var])    
  diag(cor_mat) <- 0    
  return(cor_mat[which.max(abs(cor_mat))])
}

#домашнее задание 3
smart_cor <- function(x){
  if (shapiro.test(x[[1]])$p.value < 0.05 | shapiro.test(x[[2]])$p.value < 0.05) 
    cor(x[[1]], x[[2]], method = "spearman") else cor(x[[1]], x[[2]])
}

test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
smart_cor(test_data)

# продолжение классной работы
df <- mtcars
df_numeric <- df[,c(1, 3:7)]

fit <- lm(mpg ~ hp, df)
summary(fit)   

ggplot(df, aes(hp, mpg))+
  geom_smooth(method = lm, se = F)+
  facet_grid(.~cyl)

fit_v <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)

new_df <- data.frame(hp = c(100, 150, 129, 300))
new_df$mpg <- predict(fit, new_df)

predict(fit, new_df)

df$cyl <- factor(df$cyl, labels = c("four", "six", "eight"))

fit <- lm(mpg ~cyl, df)
summary(fit)

ggplot(df, aes(cyl, mpg))+
  geom_point()+
  theme(axis.text=element_text(size=10),
        axis.title = element_text(size=20, face="bold"))


aggregate(mpg ~ cyl, df, mean)

#домашняя работа 3
df_hm <- read.table("./Datasets/dataset_11508_12.txt")
head(df_hm)
fit_h <- lm(V1 ~ V2, df_hm)
summary(fit_h)

#домашняя работа 4
df_hm2 <- diamonds
head(df_hm2)
df_hm2_sub <- subset(diamonds, diamonds$carat == 0.46 & diamonds$cut == "Ideal")
fit_h2 <- lm(price ~ depth, df_hm2_sub)
fit_coef <- fit_h2$coefficients

#домашняя работа 5
regr.calc <- function(x){
  if (abs(cor.test(x[[1]], x[[2]])$p.value) < 0.05){
    x <- cbind(x, fit = lm(x[[1]] ~ x[[2]], x)$fitted)
    return(x)
  } else print("There is no sense in prediction")
}
regr.calc(iris[,c(1,2)])

#домашняя работа 6
library(ggplot2)
my_plot <- ggplot(iris, aes(Sepal.Width, Petal.Width, color = Species))+
  geom_point()+
  geom_smooth(method = "lm")
my_plot

