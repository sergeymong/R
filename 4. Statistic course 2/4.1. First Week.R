# хи-распределение пирсона
(18-25)^2/25+(55-50)^2/50+(27-25)^2/25
#равно
chisq.test(c(18, 55, 27, 82))
qplot(x = rnorm(20), y = rnorm(20), xlim = c(-2, 2), ylim = c(-2,2))

# домашние задания по квадрату пирсона
t.test(table(sample(6, 100, replace = T)))
chisq.test(c(795, 1500-795))


students <- rbind(c(15,9), c(11, 6))
chisq.test(students)


drivers <- rbind(c(20, 15), c(11, 12), c(7, 9))
chisq.test(drivers)$expected


patients<- rbind(c(25, 1), c(3, 30))
colnames(patients) <- c("Yes", "No")
rownames(patients) <- c("Placebo", "Aspirin")
mosaicplot(patients, color=T, shade=T, ylab="Thrombosis", xlab="Group", cex.axis=1, main="")

#точный критерий фишера
chisq.test(patients)


# практические задачи на R
# задание 1
smart_test <-  function(x){
  t <- table(x)
  ifelse(any(t < 5),
         return(fisher.test(t)$p.value),
         return(unlist(chisq.test(t)[1:3]))
  )
}
#проверка задания 1
t <- as.data.frame(list(am = c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1), vs = c(0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1)))
t <- mtcars[1:20,c("am", "vs")]
smart_test(t)

# задание 2
t <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)
most_significant <-  function(x){
  # в t сохраняем вектор из p-значений по столбцам
  t <- sapply(x, function (y) chisq.test(table(y))$p.value)
  # возвращаем имена тех векторов, которые соответствуют минимальному значению, даже если их несколько
  return(names(which(t == min(t))))
}
#проверка задания 2
most_significant(t)

# задание 3
library(dplyr)

ic <- iris %>% 
  mutate_if(is.numeric, funs(. > mean(.))) %>% 
  mutate(important_cases = as.factor(ifelse(rowSums(.[1:4]) >= 3, "Yes", "No"))) %>% 
  select(important_cases)

iris1 <- cbind(iris, ic)
iris1

# задание 4
library(dplyr)
get_important_cases <- function(x){
  ic <- x %>% 
    mutate_if(is.numeric, funs(. > mean(.))) %>% 
    mutate(important_cases = as.factor(ifelse(rowSums(.) > length(colnames(.))/2, "Yes", "No"))) %>% 
    select(important_cases)
  
  cbind(x, ic)
}


get_important_cases2  <- function(d){    
  m <-  colMeans(d)    
  compare_to_means <- apply(d, 1, function(x) as.numeric(x > m))    
  is_important <- apply(compare_to_means, 2, sum) > ncol(d)/2    
  is_important <- factor(is_important, levels = c(FALSE, TRUE), labels = c('No', 'Yes'))    
  d$important_cases <- is_important    
  return(d)
}

t <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))

test_data <- as.data.frame(list(V1 = c(23, 13, 23, 18, 19, 30, 17, 27, 20, 27), V2 = c(16, 14, 19, 24, 20, 14, 23, 18, 26, 26), V3 = c(17, 14, 21, 13, 26, 13, 21, 27, 18, 20), V4 = c(26, 14, 27, 15, 23, 16, 22, 14, 20, 18)))
get_important_cases(t)

benchmark(get_important_cases(t))
benchmark(get_important_cases2(t))

# задание 5
stat_mode <- function(x){
  t <- table(x)
  as.numeric(names(which(t == max(t))))
}
# проверка задания 5
t <- c(1, 2, 3, 3, 3, 4, 5)
t <- c(1, 1, 1, 2, 3, 3, 3)
t <- c(19,10,18,16,2,14,5,19,8,1,8,2,11,3,10,14,13)
stat_mode(t)

# задание 6
t <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")
max_resid <- function(x){
  t <- chisq.test(table(x))$stdres
  return(c(rownames(t) [which.max(apply(t, 1,max))], 
           colnames(t) [which.max(apply(t, 2,max))]))
}
max_resid(t)


# задание 7
ggplot(diamonds, aes(color, fill = cut))+
  geom_bar(position = position_dodge())


