warpbreaks %>% 
  group_by(wool, tension) %>% 
  summarise(avg = mean(breaks), max = max(breaks)) %>% 
  filter(avg > 25 | max > 42)
