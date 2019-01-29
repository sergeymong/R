ggplot(mtcars, aes(mpg, hp, col = factor(am)))+
  geom_point()+
  scale_x_continuous(name = "Miles/gallon",
                     breaks = c(1, seq(10, 35, by = 5)),
                     limits = c(1, 35))

ggplot(mtcars, aes(hp, fill = factor(am)))+
  geom_density(alpha = 0.2)+
  scale_fill_manual(name = "Type of Gear", 
                    labels = c("Auto", "Manual"),
                    values = c("Green", "Red"))

# домашняя работа 1
ggplot(iris, aes(Sepal.Length, Petal.Length, col = Species))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous(name = "Длина чашелистика",
                     limits = c(4, 8))+
  scale_y_continuous(name = "Длина лепестка",
                     breaks = seq(1, 7, by = 1),
                     limits = c(1, 7))+
  scale_color_discrete(name = "Вид цветка",
                       labels = c("Ирис щетинистый", "Ирис разноцветный", "Ирис виргинский"))


#  продолжение классной работы
ggplot(mtcars, aes(factor(am), hp, fill = factor(cyl)))+
  geom_boxplot()+
  scale_fill_brewer(type = "qual", palette = 6)+
  theme_bw()


# сложный и красивый график
d <- read.csv("./Datasets/example_data.csv")
ggplot(d, aes(date, percent, col = system, group = system))+
  geom_line(alpha = 0.7)+
  geom_point(shape = 21, size = 3,fill = "black", stroke = 1.7, alpha = 0.7)+
  geom_vline(xintercept = 7.5, color = "white", linetype = "dotted")+
  scale_y_continuous(breaks = c(0, .04, .08, .11, .15), 
                     limits = c(0, .15),
                     labels = scales::percent)+
  scale_color_manual(values = c("orangered1", 
                                "red", 
                                "cyan", 
                                "yellow1", 
                                "springgreen2"))+
  xlab("") + ylab("")+
  theme_classic()+
  theme(legend.title = element_blank(),
        legend.position = "top", 
        plot.background = element_rect(color = "black",
                                       fill = "black"),
        panel.background = element_rect(color = "black",
                                        fill = "black"),
        legend.background = element_rect(color = "black",
                                        fill = "black"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        panel.grid.major.y = element_line("grey50", linetype = "longdash"),
        axis.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 13),
        legend.text = element_text(size = 13),
        title = element_text(size = 16, face = "bold"))
  
d
