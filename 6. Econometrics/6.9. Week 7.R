library(dplyr)
library(erer)
library(vcd)
library(ggplot2)
library(reshape2)
library(AUC)
library(lmtest)
library(broom)

options(stringsAsFactors = F)
t <- read.csv("./Datasets/titanic3.csv")

glimpse(t)

t <- mutate(t, sex=as.factor(sex), pcalss = as.factor(pclass),
            survived = as.factor(survived))
summary(t)

mosaic(~sex+pclass+survived,shade=T, data=t) # окраска цветом говорит о том, больше или меньше данных попадает в группу в сравнении с тем, если бы они были независимы. Красный -- меньше, синий -- больше
qplot(data=t, survived, age, geom="violin")
qplot(data=t, survived, age, geom="boxplot")

qplot(data=t, age, y = ..count.., fill = survived, geom="density", position = "density")
ggplot(data=t, aes(age, fill = survived)) + geom_density(position = 'fill')


#logit and probit models
m_logit <- glm(data=t, survived~sex+age+pclass+fare, family = "binomial"(link = "logit"), x=T)
m_probit <- glm(data=t, survived~sex+age+pclass+fare, family = "binomial"(link = "probit"), x=T)
summary(m_logit) # для мужчин и женщин и бинарных переменных, при изменении пола вероятность выжить уменьшаются на экспоненту от 2.49
summary(m_probit) # например, при увеличении возраста на 1 год, верояотность выжить уменьшается на 1.9%

# covariation matrix
vcov(m_logit) 

newdata <- data.frame(age=seq(5,100, length = 100), sex = "male", pclass="2nd", fare=100)
pr_logit <- predict(m_logit, newdata, se=T)
newdata_pr <- cbind(newdata, pr_logit)
newdata_pr

newdata_pr <- mutate(newdata_pr, prog=plogis(fit), left_ci=plogis(fit-1.96*se.fit),
                                                                  right_ci=plogis(fit+1.96*se.fit)) # cпрогнозировали вероятности для наших значений

qplot(data=newdata_pr, age, prog) + geom_ribbon(aes(ymin=left_ci, ymax=right_ci), alpha=0.2)

#compare of models  
t2 <- select(t,sex,age,survived,fare) %>% na.omit()
m_logit2 <- glm(data=t2, survived~sex+age, family = "binomial"(link = "logit"), x=T)

#LR test
lrtest(m_logit, m_logit2) #hypotesis about significance fare approved

maBina(m_logit) #marginal effects of mean man in titanic
maBina(m_logit, x.mean = F) #marginal effects of each man on Titanic and then mean value of this computings

#ROC
pr_t <- predict(m_logit,t,se=T)
t1 <- cbind(t, pr_t)
t1
t <- mutate(t, prog=plogis(fit)) #prediction for each passenger -- chance and fact: die or survived
head(t)

roc.data <- roc(t$prog, t$survived)
str(roc.data) #cutoffs - threshold of chance, fpr -- false positive rate, tpr -- true positive rate

qplot(roc.data$cutoffs, roc.data$tpr, geom="line")
qplot(roc.data$cutoffs, roc.data$fpr, geom="line")

qplot(roc.data$fpr, roc.data$tpr, geom="line")



#homework
qchisq(0.9, 1)
pnorm(0.5)
qnorm(0.975, 2)

mosaic(data=HairEyeColor,Sex~Eye+Hair, shade=TRUE)
mosaic(data=HairEyeColor,~Eye+Hair+Sex, shade=TRUE)
mosaic(data=HairEyeColor,~Hair+Sex+Eye, shade=TRUE)
mosaic(data=HairEyeColor,~Sex+Eye+Hair, shade=TRUE)


t <- read.csv("./Datasets/titanic3.csv")
t <- mutate(t, sex=as.factor(sex), pcalss = as.factor(pclass),
            survived = as.factor(survived))
model <- glm(data=t, survived ~ age  + sex + fare + sibsp, family = "binomial"(link = "logit"))
summary(model)

confint(model2, level = 0.95)

newdata <- data.frame(sex = "male", age = 50, sibsp = 2, fare = 200)

pre <- predict(model2, newdata, se.fit=T, type = "response")
pre
pre$fit - 1.96*pre$se.fit

plogis(-0.110658)

-0.110658 - 1.96*0.3478944

model2 <- glm(data=t, survived ~ age + I(age^2) + sex + fare + sibsp, family = "binomial"(link = "logit"), x=T)

maBina(model3)

d <- select(t, age, sibsp, sex, fare, parch, survived)
d <- na.omit(d)
model3 <- glm(data=t, survived ~ age + I(age^2) + sex + fare + sibsp + parch,family = "binomial"(link = "logit"), x=T)
model4 <- glm(data=d, survived ~ age + age^2 + sex + sibsp + parch)
lrt <- lrtest(model3, model4)


d <- select(t, age, sibsp, sex, fare,survived)
d <- na.omit(d)
model5 <- glm(data=d, survived ~ age + age^2 + sex + fare + sibsp)

pr_t <- predict(model5,d,se=T)
d <- cbind(d, pr_t)
d <- mutate(d, prod=plogis(fit))

roc.data <- roc(d$prod, d$survived)
roc.data
roc.data <- data.frame(roc.data$cutoffs, roc.data$tpr, roc.data$fpr)


model6 <- glm(data=t, survived ~ age + I(age^2) + sex + fare + sibsp, family = "binomial"(link="logit"))
summary(model6)
vcov(model6)


-(-0.0663291/(2*0.0007399))

d <- dplyr::select(t, age, sibsp, sex, fare,survived)

d_clean <- na.omit(d)

model <- glm(data=t, family=binomial(link="logit"), survived ~ age + I(age^2) + sex + fare + sibsp)
model2 <- glm(data=d_clean, family=binomial(link="logit"), survived ~ age + I(age^2) + sex + sibsp)


lrt <- lrtest(model, model2)
lrt
d_clean$probs <- predict(model, type="response")

d_clean

table(d_clean$survived, d_clean$probs > 0.65)


vcov(model)
