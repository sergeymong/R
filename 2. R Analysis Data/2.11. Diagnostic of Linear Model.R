library(ggplot2)

data(swiss)
str(swiss)

pairs(swiss)
ggplot(swiss, aes(Examination, Education))+
  geom_point()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 25, face = "bold"))+
  geom_smooth(method = "lm")

ggplot(swiss, aes(Examination))+
  geom_histogram()

ggplot(swiss, aes(Education))+
  geom_histogram()

my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)

hist(scale(my_vector))

shapiro.test(scale(my_vector))

#домашнее задание 1
beta.coef <- function(x){
  lm(scale(x[[1]]) ~ scale(x[[2]]))$coefficients
}

beta.coef(mtcars[,c(1,3)])

#домашнее задание 2
normality.test  <- function(x){
  sapply(x, function(y) shapiro.test(y)$p.value)
}

normality.test(mtcars[,1:6])

shapiro.test(data$Installs)

#продолжение классной работы
ggplot(swiss, aes(Examination, Education))+
  geom_point()+
  geom_smooth(method = "lm")

lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)

swiss$Examination_squared <- swiss$Examination^2

lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)

anova(lm1, lm2)


swiss$lm1_fitted <- lm1$fitted.values
swiss$lm2_fitted <- lm2$fitted.values
swiss$lm1_resid <- lm1$residuals
swiss$lm2_resid <- lm2$residuals
swiss$observ_num <- 1:nrow(swiss)

ggplot(swiss, aes(Examination, Education))+
  geom_point(size = 3)+
  geom_line(aes(Examination, lm1_fitted), col = "red", lwd=1)+
  geom_line(aes(Examination, lm2_fitted), col = "blue", lwd=1)


ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid))+
  geom_point(size = 3)+ 
  geom_hline(y = 0, col = 'red', lwd = 1)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid))+
  geom_point(size = 3)

ggplot(swiss, aes(x = observ_num, y = lm1_resid))+
  geom_point(size = 3)+ 
  geom_smooth()

ggplot(swiss, aes(x = observ_num, y = lm2_resid))+
  geom_point(size = 3)+ 
  geom_smooth()

#домашнее задание 3
df <- read.csv("homosc.csv")
summary(gvlma(DV ~ IV, df))

#домашнее задание 4
resid.norm  <- function(fit){
 x <- as.data.frame(fit$residuals)
 my_plot <-  ggplot(x, aes(fit$residuals))+
    geom_histogram(fill = ifelse(shapiro.test(fit$residuals)$p.value < 0.05, "red", "green"))
 my_plot
}

fit <- lm(mpg ~ wt, mtcars)
my_plot <- resid.norm(fit)
my_plot

#домашнее задание 5
high.corr <- function(x){
  t <- as.data.frame(cor(x))
  diag(t) <- 0
  t <- abs(t)
  res <- c(colnames(t[which(t==max(t), arr.ind = T)[2]]),
           rownames(t[which(t==max(t), arr.ind = T)[1],]))
  res
}

high.corr(my_df)

t <- as.data.frame(list(V1 = c(-1.5, -1.1, -1.4, 1.1, 0.2, -0.1, 0.2, 0.1, 1.1, 0.2, -1, -0.4, -0.6, -1.3, 0.5, 1.8, -0.9, -0.4, -1.6, 0, -2.7, -0.2, 1.2, -1, 1), V2 = c(1.5, 0.2, 1.4, 0.5, 1.8, 1.2, -1.1, 0.2, 1.4, 0.7, -0.5, 0.4, -0.2, 0, 0.6, 1.5, -1, 0.4, -1.4, -1.4, 0, 0.4, 0.4, -2.6, -0.1), V3 = c(-1.5, -0.2, -1.4, -0.5, -1.8, -1.2, 1.1, -0.2, -1.4, -0.7, 0.5, -0.4, 0.2, 0, -0.6, -1.5, 1, -0.4, 1.4, 1.4, 0, -0.4, -0.4, 2.6, 0.1)))
t <- as.data.frame(swiss)
t <- cor(t)
diag(t) <- 0
abs(t)


res <- colnames(t[round(which(t==max(t))/nrow(t))])

t[which(t==max(t)),]

rownames(t[2])

rownames(t[which(t==max(t))/nrow(t)])
rownames(t[which(t==max(t), arr.ind = T)])
res

which(t[max(t)])

colnames(t[,round(which(t==max(t))/nrow(t))])

t[,round(which(t==max(t))/nrow(t))]
t1 <- apply(t,1,function(x) names(t)[which(x==max(x))])
c(names(t1[1]),t1[[1]])

x1 <- rnorm(30) # создадим случайную выборку
x2 <- rnorm(30) # создадим случайную выборку
x3  <- x1 + 5 # теперь коэффициент корреляции x1 и x3 равен единице
my_df <- data.frame(var1 = x1, var2 = x2, var3 = x3)
t <- cor(my_df)
diag(t) <- 0
t

res <- colnames(t[,round(which(t==max(t))/(nrow(t)-1))])
