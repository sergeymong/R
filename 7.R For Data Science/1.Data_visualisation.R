library(tidyverse)

mpg
?mpg

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg)+
  geom_point(mapping = aes(x = cyl, y = hwy))

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

  
