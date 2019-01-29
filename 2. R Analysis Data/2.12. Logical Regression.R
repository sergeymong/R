df <- read.csv("./Datasets/train.csv", sep = ";")
str(df)

ggplot(df, aes(read, math, col = gender))+
  geom_point()+
  facet_grid(.~hon)

fit <- glm(hon ~ read + math + gender, df, family = "binomial")
summary(fit)

head(predict(fit, type = "response"))

df$prob <- predict(fit, type = "response")

#домашнее задание 1
hdf <- mtcars
str(hdf)
hdf$vs <- as.factor(hdf$vs)
hdf$am <- as.factor(hdf$am)
t <- glm(am ~ disp + vs + mpg, hdf, family = "binomial")
log_coef <- t$coefficients


obj <- ggplot(data = ToothGrowth, aes(as.factor(supp), len, fill = as.factor(dose)))+
  geom_boxplot()
obj

#продолжение классной работы
pred_fit <- prediction(df$prob, df$hon)
perf_fit <- performance(pred_fit, "tpr", "fpr")

auc <- performance(pred_fit, "auc")
str(auc)
plot(perf_fit, colorize = T, print.cutoffs.at = seq(0,1, by=0.1))


perf3 <- performance(pred_fit, "cutoff", measure = "spec")
perf4 <- performance(pred_fit, "cutoff", measure = "sens")
perf5 <- performance(pred_fit, "cutoff", measure = "acc")

plot(perf3, col="red", lwd = 2)
plot(add=T, perf4, col="green", lwd = 2)
plot(add=T, perf5, lwd = 2)

abline(v=0.220, lwd = 2)

df$pred_resp <- factor(ifelse(df$prob > 0.22, 1, 0), labels = c("N", "Y"))
df$correct <- ifelse(df$pred_resp == df$hon, 1, 0)
sum(df$correct)/length(df$correct)

df <- read.csv('test.csv', sep = ";")

#домашнее задание 2
hdf <- read.csv("https://stepic.org/media/attachments/lesson/11478/data.csv")
fit <- glm(admit ~ rank*gpa, hdf, family=binomial(link="logit"))

hdf$prob<- predict(fit, newdata = hdf, type = "response")
hdf$prob[is.na(hdf$admit) == F] <- hdf$admit[is.na(hdf$admit) == F]

length(hdf$prob[hdf$prob > 0.4 & is.na(hdf$admit) == T])

#продолжение классной работы
fit1 <- lm(mpg ~ cyl + disp, mtcars)
fit2 <- aov(mpg ~ am * vs, mtcars)

fit_table1 <- xtable(fit1)
fit_table2 <- xtable(fit2)

print(fit_table1, type = "html", file="fit_table1.html")
print(fit_table2, type = "html", file="fit_table2.html")

stargazer(fit1, type="html", 
          dep.var.labels ="mpg",
          covariate.labels = c('cyl', 'disp'), out = 'models1.html')
