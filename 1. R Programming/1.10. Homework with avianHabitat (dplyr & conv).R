library(tidyr)
library(dplyr)
library(stringr)
library(plyr)
library(data.table)

avian <- read.csv("avianHabitat.csv")
avian <- avian %>% mutate(Site = factor(str_replace(Site, "[:digit:]+", "")))

df <- avian %>% 
  select(Site, Observer, contains("Ht")) %>%
  setnames(., colnames(.[grepl("Ht", colnames(.))]), c("DB", "W", "E", "A", "H", "L")) %>% 
  unite(Site, Site, Observer, sep = ", ") %>% 
  gather(Type, Value, -Site) %>% 
  filter(Value > 0) %>% 
  unite(Site, Site, Type, sep = ", ") %>% 
  group_by(Site) %>% 
  count("Site")
  

# более короткая и быстрая альтернатива
avian %>% 
  select(Site, Observer, contains("Ht")) %>% 
  mutate(Site = factor(str_replace(Site, "[:digit:]+", ""))) %>% 
  group_by(Site, Observer) %>% 
  summarise_if(is.numeric, funs(sum(.>0)))
