install.packages('lme4')
install.packages('mlmRev')
install.packages('lmerTest')

library('lme4')
library('mlmRev')
library('lmerTest')

lmer(DV)

exp_data <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
ggplot(exp_data, aes(frequency, fill = subject))+
  geom_density(alpha = 0.2)+
  facet_wrap(~gender, ncol = 1)


lmer(frequency ~ attitude + gender + (1+ attitude|subject) + (1 + attitude|scenario), exp_data)
