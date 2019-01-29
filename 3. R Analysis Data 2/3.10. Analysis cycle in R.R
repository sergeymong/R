library(dplyr)
library(tidyr)
library(ggplot2)


glaicer <- read.csv("./Datasets/glacier.csv", na.strings = "..", comment.char = "#")
glaicer <- glaicer %>% 
  select(Ref_Date, GEO, MEASURE, Value) %>% 
  filter(MEASURE == "Annual mass balance") %>% 
  separate(GEO, c("Name", "Location"), sep = " - ")

#descript analytics
g1 <- glaicer %>% 
  group_by(Name) %>% 
  summarise(YearsObserved = n(),
            MeanChange = mean(Value, na.rm = T),
            WorstChange = mean(Value, na.rm = T),
            WorstYear = Ref_Date[which.min(Value)])

#t-test
g2 <- glaicer %>% 
  group_by(Name) %>% 
  do({
    tt <- t.test(.$Value, alternative = "less", mu = 0, conf.level = .99)
    data.frame(PValue = tt$p.value, ConfidenceLimit = tt$conf.int[2])
  })

left_join(g1, g2, by = "Name") %>% 
  knitr::kable(caption = "Descriptive stats and confidence intervals",
               digits = c(0,0,2,0,0,10,2))

#ggplot
ggplot(glaicer, aes(Ref_Date, Value))+
  geom_line()+
  geom_hline(data = g1, aes(yintercept = MeanChange),
             color = "red", linetype = "dashed", alpha = 0.9)+
  facet_wrap(~ Name, nrow = 2)
