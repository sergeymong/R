df <- data.frame(type = c(1, 1, 2, 2, 3, 3), value = c(5, 10, 50, 100, 7, 7))

arrange(summarize(group_by(df, type), Total = sum(value), -Total))

a <- group_by(df, type)
b <- summarize(a, Total = sum(value))
c <- arrange(b, -Total)


df %>% 
  group_by(type) %>%
  summarize(Total = sum(value)) %>%
  arrange(-Total)

