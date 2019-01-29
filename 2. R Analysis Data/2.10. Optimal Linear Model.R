swiss <- data.frame(swiss)
fit_full <- lm(Fertility ~ ., swiss)
summary(fit_full)

fit_reduce1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, swiss)
summary(fit_reduce1)

anova(fit_full, fit_reduce1)

fit_reduce2 <- lm(Fertility ~ Infant.Mortality + Agriculture + Catholic + Education, swiss)
summary(fit_reduce2)

anova(fit_full, fit_reduce2)

optimal_fit <- step(fit_full, direction = "backward")
summary(optimal_fit)

#домашнее задание 1
model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)

ideal_model <- step(model_null, scope = list(lower = model_null, upper = model_full))

anova(model_full, ideal_model)

#домашнее задание 2
full_m <- data.frame(LifeCycleSavings)

summary(lm(sr ~ ., full_m))
summary(lm(sr ~ (.)^2, full_m)) 
