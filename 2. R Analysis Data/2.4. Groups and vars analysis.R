df <- read.csv("./Datasets/grants.csv")
str(df)

df$status <- factor(df$status, labels = c("Not funded", "Funded"))

t1 <- table(df$status)
t1

t2 <- table(status = df$status, field = df$field)
t2

prop.table(t2, 1)

t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3

#домашнее задание
df1 <- as.data.frame(HairEyeColor)
dimnames(df1)
df1

#вариант 1
red_men <- df1["Red", "Blue", "Male"]/sum(df1[, "Blue", "Male"])
#вариант 2
red_men1 <- prop.table(HairEyeColor[,"Blue" ,"Male" ])["Red"]

sum(HairEyeColor[, "Green", "Female"])


#продолжение классной работы -- графики по данным
barplot(t2, legend.text = T, args.legend = list("topright"))
barplot(t2, legend.text = T, args.legend = list("topright"), beside = T)

mosaicplot(t2)

#домашнее задание 2
t3 <- subset(df1, Sex == "Female")
ggplot(t3, aes(x = Hair, y = Freq, fill = Eye))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))


#продолжение классной работы -- тесты
binom.test(x = 5, n = 100, p = 0.1)
binom.test(t1)

chisq.test(t1)
chi <- chisq.test(t1)
chi$obs

chisq.test(t2)$exp
fisher.test(t2)


#домашнее задание 3
t <- as.data.frame(HairEyeColor)
hec <- HairEyeColor["Brown", "Female"]


#домашнее задание 4
#вариант 1
t <- diamonds
tpm <- mean(diamonds$price)
tcm <- mean(diamonds$carat)
t$factor_price <- sapply(diamonds$price, function(x) if (x >= tpm) 1 else 0)
t$factor_carat <- sapply(diamonds$carat, function(x) if (x >= tcm) 1 else 0)
main_stat <- chisq.test(x = t$factor_price, y = t$factor_carat)$stat

#вариант 2
diamonds$factor_price <- factor(ifelse(diamonds$price >= mean(diamonds$price), 1, 0))    
diamonds$factor_carat <- factor(ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0))    
main_stat_master <- chisq.test(diamonds$factor_price, diamonds$factor_carat)$statistic

#вариант 3
main_stat <- chisq.test(as.integer(diamonds$price >= mean(diamonds$price)), 
                        as.integer(diamonds$carat >= mean(diamonds$carat)))$statistic


#домашнее задание 5
ft <- fisher.test(as.factor(mtcars$am), as.factor(mtcars$vs)) #можно и без приведения к фактору
fisher_test <- ft$p.value

