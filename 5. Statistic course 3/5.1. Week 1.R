library(ggplot2)

qplot(hp, mpg, data = mtcars)
qplot(hp^0.5, mpg, data = mtcars)
qplot(-hp^-0.5, mpg, data = mtcars)

fit1 <- lm(mpg ~ hp, mtcars)
fit2 <- lm(mpg ~ I(-hp^-0.7), mtcars)
summary(fit1)
summary(fit2)

qplot(log(mpg), log(hp), data = mtcars)

fit3 <- lm(mpg ~ hp, mtcars)
summary(fit3)

plot(fit3)

library(dplyr)
set.seed(42)

d <- data_frame(y = rnorm(30), x_1 = rnorm(30), x_2 = x_1, x_3 = rnorm(30))

pairs(d)
fit <- lm(y ~ ., d)
summary(fit)

select(d, -y)


data(cars)
qplot(speed, dist, data = cars)
cars <- mutate(cars, speed_2 = speed^2, speed_3 = speed^3)

fit2 <- lm(dist ~ speed, select(cars, -speed_2))
summary(fit2)
vif(fit2)


fit1 <- lm(Fertility ~ ., swiss)
summary(fit1)

cor.test(~ Fertility + Education, swiss)
vif(fit1)

fit2 <- lm(Fertility ~ ., select(swiss, -Examination))
summary(fit2)


#дз1
library(lmtest)
library(dplyr)
hetero_test <-  function(d){
  formula <- as.formula(paste(names(d)[1], "~ ."))
  fit <- lm(formula, d)
  summary(lm(fit$residuals^2 ~ ., select(d, names(d)[-1])))$r.squared
}
hetero_test(mtcars)


#дз2
VIF <-  function(data){
  model <- lm(data[,1] ~ ., as.data.frame(data[,-1]))
  model
  # r <- sapply(2:ncol(data), function(x) summary(lm(data[,x] ~ ., as.data.frame(data[,-c(1, x)])))$r.squared)
  # vifs <- 1/(1-r)
  # names(vifs) <- names(data[,-1])
  # vifs
}

VIF <- function(data)
{
  sapply(names(data[-1]), function(name) {
    m = lm(as.formula(paste(name, "~ .")), data[-1])
    r2 = summary(m)$r.squared
    1 / (1 - r2)})
}
test_data <- as.data.frame(list(X2 = c(-1.2, 0.1, -1.5, 1, -2, 0.4, 1, -1.3, -1.6, -2), X3 = c(1.1, -0.8, 0.3, -0.1, 0.9, -2.1, 2, -0.7, -0.1, -0.6)))
VIF(test_data)
VIF(mtcars)

#дз3
smart_model <-  function(data){
  while(ncol(data) > 2 & any(VIF(data) > 10)){
    delete <- ifelse(any(VIF(data)>10), which.max(VIF(data))+1, NULL)
    data <- data[,-delete]
  }
  lm(data[,1] ~ ., as.data.frame(data[,-1]))$coefficients
}

smart_model2 <-  function(test_data){
  varVif <- VIF(test_data)
  if(any(varVif>10)){
    i <- which.max(varVif)+1 
    print(names(test_data)[i])
    smart_model(test_data[,-i])
  } else {
    lm(as.formula(paste0(names(test_data)[1],"~.")), data=test_data)$coefficients
  }
}


test_data <- data.frame(y = rnorm(30, 5), x1 = rnorm(30, 5))
test_data$x2 <- test_data$x1^2
smart_model(test_data)
smart_model(mtcars)

#дз4
transform_x <-  function(test_data){
  degrees <- seq(-2, 2, by=0.1)
  cors <- sapply(degrees, function(x)
    if (x > 0) cor(test_data[1], test_data[2]^x) else
      if (x == 0) cor(test_data[1], log(test_data[2])) else
        cor(test_data[1], -test_data[2]^x))
  
  best_degree <- degrees[which.max(abs(cors))]
  
  if(best_degree > 0) test_data[[2]]^best_degree else
    if(best_degree == 0) log(test_data[[2]]) else
      -test_data[[2]]^best_degree
}

transform_x_2 = function(data)
{
  do_transform = function(x, lambd) {
    if (lambd > 0) x ^ lambd else if (lambd < 0) -(x ^ lambd) else log(x) }
  
  x_data = data[,2] 
  y_data = data[,1]
  lambdas = seq(-2, 2, 0.1)
  corrs = sapply(lambdas, 
                 function(lambd) cor(do_transform(x_data, lambd), y_data))
  lambda = lambdas[which.max(abs(corrs))]
  #print(lambda)
  do_transform(x_data, lambda)
}

transform_x3 <- function(test_data) {
  y <- test_data[[1]]
  x <- test_data[[2]]
  # Диапазон значений лямбда
  l <- seq(-2, 2, 0.1)
  # Трансформация
  d <- outer(x, l, "^")
  d
  # Изменяем знак
  d[, l < 0] <- -d[, l < 0]
  # Для случая, где лямбда равна нулю
  d[, l == 0] <- log(x)
  
  # Расчёт корреляций
  r <- cor(d, y)[, 1]
  which.max(abs(r))
  # Выбор оптимального значения
  d[,which.max(abs(r))]
}

transform_x3(test_data)

transform_x4 <- function(df, bounds = c(-2, 2), step = 0.1){
  vec <- round(seq(bounds[1], bounds[2], step), 1)
  names(vec) <- round(seq(bounds[1], bounds[2], step), 1)
  vc <- c()
  
  #print(vec == 0)
  
  for (i in vec){
    if (isTRUE(all.equal(0, i))) a <- log(df$x)
    else a <- i/abs(i)*df$x^i
    df <- cbind(df, a)
    vc <- c(vc, abs(cor( df[,c(1, ncol(df))] ))[1,2])
  }
  # print(vc)
  # print(vc[which(vc == max(vc))])
  
  return(df[,which(vc == max(vc))+2])
}

set.seed(42)
test_data <- data.frame(y = rnorm(10, 10, 1), x = rnorm(10, 10, 1))  
test_data <- as.data.frame(list(y = c(0.4, 0.42, 0.38, 0.4, 0.41, 0.39, 0.35, 0.39, 0.4, 0.46, 0.4, 0.36, 0.42, 0.43, 0.4, 0.44, 0.36, 0.42, 0.47, 0.39), x = c(10.51, 8.75, 11.54, 10.44, 9.43, 11.49, 14.22, 11.22, 10.14, 7.07, 10.2, 13.5, 8.81, 8.69, 10.73, 7.93, 13, 8.81, 7.34, 11.32)))
transform_x(test_data)


benchmark(transform_x(test_data), replications = 100)
benchmark(transform_x_2(test_data), replications = 100)
benchmark(transform_x3(test_data), replications = 100)
benchmark(transform_x4(test_data), replications = 100)








