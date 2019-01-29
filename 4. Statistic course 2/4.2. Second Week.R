# логистическая регрессия
library(dplyr)
library(ggplot2)
library(vcd)

titanic <- read.csv("https://stepic.org/media/attachments/course/524/train.csv")
titanic <- na.omit(titanic)
glimpse(titanic)
titanic <- mutate(titanic, 
                  Survived = factor(Survived, labels = c("No", "Yes")), 
                  Pclass = factor(Pclass, labels = c("First", "Second", "Third")), 
                  Sex = factor(Sex, labels = c("Female", "Male")))

simple_fit <- glm(Survived ~ 1, titanic, family = "binomial")
coef(simple_fit)
table(titanic$Survived)
odds = table(titanic$Survived)[2]/table(titanic$Survived)[1]
log(odds)
summary(simple_fit)
exp(coef(simple_fit))

# рассчитываем вероятность из натурального логарифма
chance_from_log <- function(logariphm){
  x <- exp(logariphm)
  x/(x+1)
}

fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
summary(fit1)
coef(fit1)
table(titanic$Survived, titanic$Sex)




#модели с двумя номинативными предикторами
fit2 <- glm(Survived ~ Sex * Pclass, titanic, family = "binomial")
coef(fit2)


summary(fit2)

table(titanic$Survived, titanic$Pclass, titanic$Sex)

chance_from_log(1.61)


#модели с различными типами предикторов
fit3 <- glm(Survived ~ 1, titanic, family = "binomial")
coef(fit3)
summary(fit3)

plot(predict(fit3, type = 'response'))


# домашние задания

#дз1
test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
test_data <- transform(test_data, x = factor(x), y = factor(y)) 
get_coefficients <- function(dataset){
  formula <- paste(names(dataset)[2], " ~ ", names(dataset)[1])
  exp(coef(glm(formula, dataset, family = "binomial")))
}

get_coefficients(test_data)

#дз2
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
var_names = c("X4", "X2", "X1")

centered <- function(test_data, var_names){
  test_data[var_names] <- sapply(test_data[var_names], function(x) x-mean(x))
  test_data
}
centered(test_data, var_names)

#дз3
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv")
get_features <- function(dataset){
  fit <- glm(is_prohibited ~ ., dataset, family = "binomial")
  anv <- anova(fit, test = "Chisq")
  if (length(rownames(anv[which(anv$`Pr(>Chi)`<0.05),])) == 0) "Prediction makes no sense" else rownames(anv[which(anv$`Pr(>Chi)`<0.05),])
}
get_features(test_data)

#дз4
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")

most_suspicious <- function(dataset, data_p){
  fit <- glm(is_prohibited ~ ., dataset, family = "binomial")
  data_p$chance <- predict.glm(fit, data_p)
  data_p$passangers[which(data_p$chance == max(data_p$chance))]
}

most_suspicious(test_data, data_for_predict)

#дз5
normality_test <- function(dataset){
  data <- dataset[sapply(dataset, function(x) is.numeric(x))]
  sapply(data, function(x) shapiro.test(x)$p.value)
}

#более короткое решение
normality_test <- function(dataset){    
  sapply(dataset[sapply(dataset, is.numeric)], function(x) shapiro.test(x)$p.value)    
}

normality_test(iris)


#дз6
test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")
smart_anova <- function(data){
  shapiro_res <- sapply(levels(data$y), function(z) shapiro.test(data$x[data$y == z])$p.value)
  if(bartlett.test(data$x, data$y)$p.value < 0.05 | any(shapiro_res < 0.05)){
    c("KW" = kruskal.test(x ~ y, data)$p.value)
  } else {
    fit <- aov(x ~ y, data)
    c("ANOVA" = summary(fit)[[1]]$'Pr(>F)'[1])
  }
}

smart_anova(test_data)

#дз7
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_for_norm.csv")
normality_by <- function(test){
  test %>%
    group_by_at(c(2,3)) %>%
    summarise_each(funs(p_value = shapiro.test(.)$p.value))
}

normality_by(test_data)

#дз8

ggplot(iris, aes(Sepal.Length, fill = Species))+
  geom_density(alpha = 0.2) 
