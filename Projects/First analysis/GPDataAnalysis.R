library(ggplot2)
library(dplyr)
library(stringr)

data_raw <- read.csv("/Users/sergeymong/Downloads/Work/Personal/Programming/R/R Analysis Data 2/BTR/googleplaystore.csv", stringsAsFactors = F)

# преобразуем данные
data_raw$Reviews <- as.numeric(data_raw$Reviews)

data_raw$Category <- as.factor(data_raw$Category)

data_raw$Size <- gsub("Varies with device", NA, data_raw$Size)
data_raw$Size <- gsub("[:digits:]*k", "", data_raw$Size)
t <- grepl(pattern = "[:digits:]*M", data_raw$Size)
data_raw$Size <- gsub("[:digits:]*M", "", data_raw$Size)
data_raw$Size <- as.numeric(data_raw$Size)
data_raw$Size[t] <- data_raw$Size[t] * 1024

data_raw$Installs <- as.numeric(gsub("\\D", "", data_raw$Installs))

data_raw$Type <- as.factor(gsub("NaN|0", "Free", data_raw$Type))
data_raw$Content.Rating <- as.factor(data_raw$Content.Rating)

data_raw$Price <- as.numeric(gsub("\\$", "", gsub("Everyone", "0", data_raw$Price)))

data_raw$Current.Ver <- as.numeric(str_match(data_raw$Current.Ver, "\\d*\\.\\d{1,2}"))

data <- data_raw %>%
  select(-Android.Ver, -Last.Updated) %>%  #Возможно, надо оставить жанры, чтобы понять самые популярные внутри категории
  filter(Installs > 10000 & Current.Ver > 0 & Size > 0 & Reviews > 50) %>% 
  mutate(Length.App.Name = nchar(App))

data <- data %>%
  group_by(App) %>% 
  slice(which.max(Reviews)) %>% 
  arrange(-Installs)


data


lm(Installs ~ (Rating*Reviews*Size*Price*Current.Ver*Length.App.Name), data = data)


model_full <- lmer(Installs ~ (Rating*Reviews*Size*Price*Current.Ver*Length.App.Name), data) 
summary(model_full)
model_null <- glm(Installs ~ 1, data)

ideal_model <- step(model_null, scope = list(lower = model_null, upper = model_full))
summary(ideal_model)

data[c(3:6, 8, 11, 12), ]


# Формируем выборку с нужными нам значениями
t <- data %>% 
  group_by(Category) %>% 
  summarise(Installs = sum(Installs), 
            Mean.price = mean(Price), 
            Percent.free.apps = sum(Type == "Free")/n()*100,
            Length.of.App.Name = mean(nchar(App)),
            Mean.rating = mean(Rating, na.rm = T),
            Mean.version = mean(Current.Ver, na.rm = T),
            Num.of.apps = n(),
            Downloads.on.app = Installs/Num.of.apps) %>% 
  arrange(-Installs)

knitr::kable(t, caption = "Основные метрики по категориям") #here we indicate, how much digits will be after dot

t1 <- data %>% 
  group_by(Type) %>% 
  summarise(Installs = sum(Installs), 
            Mean.price = mean(Price), 
            Percent.free.apps = sum(Type == "Free")/n()*100,
            Length.of.App.Name = mean(nchar(App)),
            Mean.rating = mean(Rating, na.rm = T),
            Mean.version = mean(Current.Ver, na.rm = T),
            Num.of.apps = n(),
            Downloads.on.app = Installs/Num.of.apps)

t2 <- data %>% 
  group_by(Category) %>% 
  slice(which.max(Installs))

range(t2$Installs)[1] - range(t2$Installs)[2]
range(data$Installs)[1] - range(data$Installs)[2]

t3 <- data %>% 
  mutate(Genres = sub(".*;", "", Genres)) %>% 
  group_by(Category, Genres) %>% 
  summarise(Installs = sum(Installs), 
            Num.of.apps = n(),
            Downloads.per.app = Installs/Num.of.apps,
            Mean.price = mean(Price), 
            Length.of.App.Name = mean(nchar(App)),
            Mean.rating = mean(Rating, na.rm = T),
            Mean.version = mean(Current.Ver, na.rm = T)) %>% 
  arrange(-Downloads.per.app, -Installs)

t4 <- data %>%
  filter(Category == "GAME") %>% 
  mutate(Genres = sub(".*;", "", Genres)) %>% 
  group_by(Category, Genres) %>% 
  summarise(Installs = sum(Installs), 
            Num.of.apps = n(),
            Downloads.per.app = Installs/Num.of.apps,
            Mean.price = mean(Price), 
            Length.of.App.Name = mean(nchar(App)),
            Mean.rating = mean(Rating, na.rm = T),
            Mean.version = mean(Current.Ver, na.rm = T)) %>% 
  arrange(-Downloads.per.app, -Installs)


ggplot(t4, aes(Genres ,Installs, fill = Genres))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")+
  geom_col()

t4$Genres[which.max(t4$Num.of.apps)]



qplot(t4$Genres, t4$Installs, geom = "col") + theme(axis.text.x = element_text(angle = 90), axis.title.x = element_text("Genres"))

t5 <- data %>%
  filter(Category == "GAME") %>% 
  select(-Reviews) %>% 
  distinct() %>% 
  mutate(Genres = sub(".*;", "", .$Genres)) %>% 
  group_by(Category, Genres) %>% 
  arrange(-Installs)

lm_t5 <- lm(Installs ~ Rating + Size + Price + Current.Ver + Length.App.Name, t5[5:nrow(t5),])

summary(lm_t5)

ggplot(t5[5:nrow(t5),], aes(Rating, Installs))+
  geom_point()+
  geom_smooth(method = "lm")

data %>%
  filter(Category == "FAMILY" | Category == "GAME") %>% 
  group_by(Category, Content.Rating) %>% 
  tally(sort = T)

wilcox.test(data$Installs[data$Type == "Free"], data$Installs[data$Type == "Paid"])

# Проверяем статистическую значимость того, 
# что количество скачиваний по категориям действительно разное
t.test(t$Installs) # тут же можно выявить разницу по другим переменным
t.test(t$Mean.price)
t.test(t$Mean.rating)

# Проверяем корреляцию между длиной и количеством установок
cor.test(~ Length.of.App.Name + Installs, t)
# Проверяем корреляцию между ценой и количеством установок по категориям приложений
cor.test(~ Mean.price + Installs, t) 
cor.test(~ Mean.rating + Installs, t) # видим, что не зависит от категорий

t.test(t1$Installs) # тут же можно выявить разницу по другим переменным
t.test(t1$Mean.price)
t.test(t1$Mean.rating)
t.test(t1$Length.of.App.Name)


# Проверяем корреляцию между ценой и количеством установок по монетизации
cor.test(~ Mean.price + Installs, t) 
cor.test(~ Mean.rating + Installs, t)
cor.test(~ Num.of.apps + Installs, t)


cor.test(~ Price + Installs, data) # Цена никак не коррелирует с количеством скачиваний
cor.test(~ Rating + Installs, data) # Рейтинг незначительно влияет на количество скачиваний
cor.test(~ Length.App.Name + Installs, data) # Длина названия незначительно влияет на количество скачиваний
cor.test(~ Current.Ver + Installs, data) # Версия не коррелирует с количеством скачиваний
cor.test(~ Current.Ver + Rating, data) # При этом версия коррелирует с рейтингом
cor.test(~ Size + Installs, data) # Размер коррелирует с количеством скачиваний
cor.test(~ Reviews + Installs, data)
cor.test(~ Price + Rating, data) # Цена незначительно влияет на рейтинг


genres_fam <- data_raw %>% 
  group_by(Category) %>% 
  filter(Category == "FAMILY") %>% 
  select(Genres)

genres_fam <- unique(sapply(genres_fam[2], function(x) unlist(strsplit(x, ";"))))

genres_gam <- data_raw %>% 
  group_by(Category) %>% 
  filter(Category == "GAME") %>% 
  select(Genres)

genres_gam <- unique(sapply(genres_gam[2], function(x) unlist(strsplit(x, ";"))))





