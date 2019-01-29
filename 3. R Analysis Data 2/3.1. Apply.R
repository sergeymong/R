data(diamonds)
str(diamonds)

d <- matrix(rnorm(30), nrow = 5)
d

apply(d, 2, sd)

set.seed(42)
d <- as.data.frame(matrix(rnorm(30), nrow = 5))

t <- as.data.frame(list(V1 = c(-9.7, -10, -10.5, -7.8, -8.9), V2 = c(NA, -10.2, -10.1, -9.3, -12.2), V3 = c(NA, NA, -9.3, -10.9, -9.8)))

get_negative_values <- function(test_data){
  x <- apply(t2, 2, function(x) x[x < 0 & is.na(x) == F])
  res <- x[sapply(x, length) > 0]
  res
}

get_negative_values(t2)

norm_test <- apply(iris[, 1:4], 2, function(x) shapiro.test(x))
norm_test$Sepal.Length$p.value

#домашняя работа 1
na_rm_repl  <- function(x){
  as.data.frame(apply(x, 2, function(x) replace(x, is.na(x), mean(x, na.rm = T))))
}
na_rm_ifel  <- function(test_data){
  as.data.frame(apply(test_data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
}
test_data <- as.data.frame(list(V1 = c(9, 9, 9, NA, NA, 12, 5, 11, 11, 12, 13, 11, 8, 11, 11), V2 = c(NA, 6, NA, 10, 11, 11, 10, 8, 10, 10, 14, 8, 10, 10, 10), V3 = c(10, 8, 9, 9, NA, 13, 10, 8, 9, 9, 7, 8, 10, 15, 9), V4 = c(9, NA, 9, 10, 9, 8, 9, 9, 11, 9, 11, 13, 9, 14, 7), V5 = c(NA, 11, 13, 8, 10, 7, 9, 8, 10, 11, 12, 12, 12, 8, 9)))
test_data <- rbind(test_data, test_data)

benchmark(sapply(test_data, mean), replications = 500)
benchmark(vapply(test_data, mean, FUN.VALUE = numeric(1)), replications = 500)


#продолжение классной работы
my_list <- list(x = rnorm(30), y = rnorm(10))
str(my_list)

lapply(my_list, mean)

#домашняя работа 2
positive_sum <-  function(test_data){
  lapply(test_data, function(x) ifelse(length(x[x>0]) == 0, 0, sum(x[x>0], na.rm = T)))
}
d <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))
d[d>0]
positive_sum(d)

by(iris[1:4], iris$Species, colMeans) 

#домашняя работа 3
my_names <- function (dataset, names){
 
  test_data[grepl(paste(names, collapse = "|"), test_data$name),]
}

#домашняя работа 4

#мой вариант
library(dplyr)
library(lazyeval)
find_outliers <- function(t){
  num_var <- names(which(sapply(t, is.numeric)))
  tt <- t %>% 
    group_by_(.dots = names(which(sapply(t,is.factor)))) %>% 
    mutate_(is_outlier = interp(~ifelse(abs(var - mean(var)) > sd(var)*2, 1, 0), var = as.name(num_var)))
}

#вариант без доп.пакетов
find_outliers2 <- function(data) {
  num_cols <- sapply(data, is.numeric)
  splitted <- split(data[, num_cols], data[!num_cols])
  outliers <- lapply(splitted, function(x) as.numeric(abs(x - mean(x)) > sd(x) * 2))
  data$is_outlier <- unsplit(outliers, data[!num_cols])
  data <- as.data.frame(data)
  print(data)
  data
}

#проверка дз 4
test_data <- read.csv("https://stepic.org/media/attachments/course/724/hard_task.csv")
correct_answer <- read.csv("https://stepic.org/media/attachments/course/724/hard_task_ans.csv")
test_data <- find_outliers(test_data)

data("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
ToothGrowth  <- find_outliers2(ToothGrowth)
clear_data <- subset(ToothGrowth, is_outlier == 0)


#домашняя работа 5
smart_lm <- function(x){
  pred <- paste(names(which(sapply(x, function(y) (shapiro.test(y)$p.value > 0.05)) == T)[-1]), collapse = " + ")
  dep <- names(which(sapply(x, function(y) (shapiro.test(y)$p.value > 0.05)) == T)[1])
  if (is.na(pred) | is.na(dep)) 
    "There are no normal variables in the data" else 
      lm(as.formula(paste(dep, pred, sep = " ~ ")), x)$coefficients
}
# Альтернатива. Функция lm по умолчанию считает, что первая переменная целевая, а остальные предикторы.
smart_lm2 <- function(df){
  is.normal <- sapply(df[-1], function(x) shapiro.test(x)$p > 0.05)
  if (sum(is.normal) > 0) {
    lm(df[c(T, is.normal)])$coeff
  } else {
    "There are no normal variables in the data"
  }
}

#проверка дз 5
smart_lm(swiss)
test_data <- read.csv("https://stepik.org/media/attachments/course/724/test.csv")
smart_lm(test_data)
test_data <- data.frame(x = 1:100, y = 1:100, z = 1:100)


# домашнее задание 6
one_sample_t <- function(test_data, general_mean){
  lapply(test_data[sapply(test_data, is.numeric)], 
         function(x) c(t.test(x, mu = general_mean)$stat, 
                       t.test(x, mu = general_mean)$param, 
                       t.test(x, mu = general_mean)$p.value))
}
#или, если упрощать кол-во строк
one_sample_t <- function(test_data, general_mean){
  lapply(test_data[sapply(test_data, is.numeric)], 
         function(x) unlist(t.test(x, mu = general_mean)[c("statistic", "parameter", "p.value")]))
}
x <- lapply(test_data[sapply(test_data, is.numeric)], function(x) c(t.test(x, mu = 4)$stat, t.test(x, mu = 4)$param, t.test(x, mu = 4)$p.value))

one_sample_t(iris[, 1:4], 4)

normality_tests <- lapply(iris[, 1:4], shapiro.test)
normality_tests

get_p_value <- function(test_list){
  x <- lapply(test_list, function(x) x$p.value)
  x
}
get_p_value(normality_tests)


normality_tests$Sepal.Length["p.value"]


