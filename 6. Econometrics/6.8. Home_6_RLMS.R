library(devtools)
library(rlms)
library(ggplot2)
library(lmtest)
library(sandwich)

data <- rlms_read("r22i_os26c.sav")
sdata <- data %>%
  select(rj13.2, rh6, rh5, r_diplom, status, rj1.1.1) %>%
  mutate(age = 2013-rh6) %>%
  filter(status == 1 | status == 2,
         rj1.1.1 == 1e+00 | rj1.1.1 == 2e+00 | is.na(rj1.1.1) == T,
         r_diplom != 1e+08 | is.na(r_diplom) == T)

sdata$live_city <- sapply(sdata$status, function(x) if(any(x == 2, na.rm = T)) 1 else 0)
sdata$live_obl <- sapply(sdata$status, function(x) if(any(x == 1, na.rm = T)) 1 else 0)
sdata$full_satt <- sapply(sdata$rj1.1.1, function(x) if(any(x == 1, na.rm = T)) 1 else 0)
sdata$part_satt <- sapply(sdata$rj1.1.1, function(x) if(any(x == 2, na.rm = T)) 1 else 0)
sdata$man <- sapply(sdata$rh5, function(x) if(any(x == 1, na.rm = T)) 1 else 0)
sdata$woman <- sapply(sdata$rh5, function(x) if(any(x == 2, na.rm = T)) 1 else 0)
sdata$ed_mid_ne <- sapply(sdata$r_diplom, function(x) if(any(x == 1 | x == 2 | x == 3, na.rm = T)) 1 else 0)
sdata$ed_mid_e <- sapply(sdata$r_diplom, function(x) if(any(x == 4, na.rm = T)) 1 else 0)
sdata$ed_mid_sp <- sapply(sdata$r_diplom, function(x) if(any(x == 5, na.rm = T)) 1 else 0)
sdata$ed_high <- sapply(sdata$r_diplom, function(x) if(any(x == 6, na.rm = T)) 1 else 0)

sdatawna <- na.omit(sdata)

max(sdata$age)

sum(is.na(sdata$rj13.2))

qplot(sdatawna$rj13.2) + geom_vline(xintercept = 50000)

ggplot(sdatawna, aes(x = rj13.2/1000))+
  geom_histogram(bins = 30, col = "grey", fill = "blue")+
  facet_wrap(~rh5)


model <- lm(data=sdatawna, rj13.2 ~ age + man + ed_mid_e + ed_mid_sp + ed_high + live_city + full_satt)
summary(model)

vcovHC(model)


sdata$status
