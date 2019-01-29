ggplot(diamonds, aes(carat, fill = cut))+
  geom_density(alpha = 0.6)+
  facet_grid(color ~ cut)

mtcars <- mutate(mtcars,
                 am = factor(am, labels = c("A", "M")),
                 vs = factor(vs, labels = c("V", "S")))


ggplot(mtcars, aes(hp, mpg))+
  geom_point(aes(color = factor(cyl)))+
  facet_grid(am ~ ., margins = F)+
  geom_smooth(method = "lm")

ggplot(diamonds, aes(carat, fill = cut))+
  geom_density(alpha = 0.6)+
  facet_wrap(~ color, ncol = 3)

# домашняя работа 1, 2, 3, 4
ggplot(mtcars, aes(mpg))+
  geom_dotplot()+
  facet_grid(am ~ vs)

ggplot(iris, aes(Sepal.Length))+
  geom_density()+
  facet_wrap(~ Species)

ggplot(iris, aes(Sepal.Length, Sepal.Width))+
  geom_point()+
  facet_wrap(~ Species)+
  geom_smooth()

movies <- read.csv("https://stepik.org/media/attachments/course/724/myMovieData.csv")
str(movies)

ggplot(movies, aes(Type, Budget))+
  geom_boxplot()+
  facet_grid(. ~ Year)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # переворачивает подписи оси х на 90 градусов
