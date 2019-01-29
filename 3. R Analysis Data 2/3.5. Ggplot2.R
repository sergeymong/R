# разбор qplot
data("diamonds")

qplot(price, data = diamonds)
qplot(price, carat, data = diamonds)
qplot(cut, carat, data = diamonds)

depth_hist <- qplot(diamonds$depth)
depth_hist

qplot(price, 
      carat,
      color = color,
      data = diamonds,
      geom = "point")

qplot(mpg, 
      hp,
      color = factor(am),
      shape = factor(cyl),
      size = I(4),
      data = mtcars)

price_carat_clarity_points <- qplot(carat, price, color = clarity, data = diamonds)
price_carat_clarity_points

qplot(color, 
      price,
      data = diamonds,
      color = cut)

# разбор ggplot
ggplot(diamonds, aes(price, carat))+
  geom_point(aes(color = cut))+
  geom_smooth()

gr_airq <- airquality %>% 
  group_by(Month) %>% 
  summarise(mean_temp = mean(Temp), mean_wind = mean(Wind))

ggplot(gr_airq, aes(Month, mean_temp))+
  geom_line()+
  geom_point(aes(size = mean_wind), color = "red")+
  geom_hline(yintercept = 75)


gr_mtcar <- mtcars %>% 
  group_by(am, vs) %>% 
  summarise(mean_mpg = mean(mpg), y_max = mean(mpg) + 1.96*sd(mpg)/sqrt(length(mpg)),
            y_min = mean(mpg) - 1.96*sd(mpg)/sqrt(length(mpg)))


ggplot(gr_mtcar, aes(factor(am), mean_mpg, col = factor(vs), group = factor(vs)))+
  geom_line()+
  geom_errorbar(aes(ymin = y_min, ymax = y_max), width = 0.2)+
  geom_point(shape = 21, size = 3, fill = "white")
  
ggplot(mtcars, aes(factor(am), mpg))+
  stat_summary()

#домашнее задание 1
ggplot(mtcars, aes(factor(am), mpg))+
  geom_violin()+
  geom_boxplot(width = 0.2)

#домашнее задание 2, 3, 4
sales = read.csv("https://stepic.org/media/attachments/course/724/sales.csv")
str(sales)

ggplot(sales, aes(income, sale))+
  geom_point(aes(col = shop))+
  geom_smooth(method = "lm")

ggplot(sales, aes(shop, income, col = season))+
  stat_summary(fun.data = mean_cl_boot, 
               geom = "pointrange",
               position = position_dodge(0.2))

ggplot(sales, aes(date, sale, col = shop))+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(0.2)) + # добавим стандартную ошибку
  stat_summary(fun.data = mean_cl_boot, geom = "point", position = position_dodge(0.2)) + # добавим точки
  stat_summary(fun.data = mean_cl_boot, geom = "line", position = position_dodge(0.2)) # соединим линиями


