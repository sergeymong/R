library(data.table)
library(dplyr)
library(ggplot2)
library(magrittr)
library(formattable)
library(outliers)


data_raw <- fread("BlackFriday.csv")
head(data_raw)
glimpse(data_raw)
glimpse(data)

#checking columns to NA
data.frame("col_num" = seq(1, length(names(data_raw))),
           "freq" = sapply(data_raw, function(x) sum(is.na(x))))

#checking columns to convert as factor
data.frame("col_num" = seq(1, length(names(data_raw))),
  "freq" = sapply(data_raw, function(x) length(unique(x))))



# turning some columns into factor and remove NA columns. Preprocess
data <- data_raw %>% 
  mutate_at(c(3:9), funs(factor(.))) %>% 
  select(which(colSums(is.na(.)) == 0))

levels(data$Gender) <- c("Female", "Male")
levels(data$Marital_Status) <- c("Not Married", "Married")


sample_by_user <- data %>%
  group_by(User_ID, Gender, Age, Marital_Status, Stay_In_Current_City_Years, City_Category) %>% 
  summarise(purchases = n(), 
            sum_p_thous = sum(Purchase)/1000,
            avg_purch_thous = sum_p_thous/purchases) %>% 
  arrange(-sum_p_thous)

#  Видим, что у нас три последних значения сильно отклоняются от выборки -- это выборсы, избавимся от них
qqnorm(sample_by_user$sum_p_thous)

#удалили выбросы
sample_by_user <- sample_by_user[-1:-3,]

# Создали ранг для наших покупаетелей, к какой категории они относятся
sample_by_user$vol_of_purch <- cut(sample_by_user$sum_p_thous, 3, labels = c("Low", "Medium", "High"), ordered_result = T)


# График, в котором видим, что женщины покупают меньше вещей, чем мужчины
ggplot(sample_by_user, aes(purchases, fill = Gender))+
  facet_wrap(~Gender, ncol = 1, scales = "free")+
  geom_density()+
  geom_vline(data=filter(sample_by_user, Gender=="Female"), 
             aes(xintercept = mean(sample_by_user$purchases[sample_by_user$Gender == "Female"])))+
  geom_vline(data=filter(sample_by_user, Gender=="Male"), 
             aes(xintercept = mean(sample_by_user$purchases[sample_by_user$Gender == "Male"])))+
  geom_text(data=filter(sample_by_user, Gender=="Female"),
            aes(x = mean(sample_by_user$purchases[sample_by_user$Gender == "Female"])+0.5, y = 0.01, label="\nMean female"), 
            angle=90, size=4)+
  geom_text(data=filter(sample_by_user, Gender=="Male"),
            aes(x = mean(sample_by_user$purchases[sample_by_user$Gender == "Male"])+0.5, y = 0.01, label="\nMean male"), 
            angle=90, size=4)+
  scale_y_continuous(breaks = seq(0, 1, by = 0.0025), 
                     limits = c(0, 0.0145), 
                     labels = scales::percent)+
  scale_x_continuous(breaks = seq(0, 1100, by = 100), 
                     limits = c(0, 1100))+
  labs(title = "Распределение покупок по полу ")

# График, в котором видим, что женщины покупают меньше вещей, чем мужчины
fun_mean <- function(x) return(data.frame(y=mean(x), label=format(mean(x,na.rm=T), digits = 2)))
ggplot(sample_by_user, aes(Gender, purchases, fill = Gender))+
  geom_boxplot()+
  stat_summary(fun.data = fun_mean, geom="text", vjust = 1)+
  labs(title = "Количество покупок по полу")

# Незамужние покупают больше
ggplot(sample_by_user, aes(Gender, purchases, fill = Gender))+
  geom_boxplot()+
  facet_grid(~Marital_Status)+
  stat_summary(fun.data = fun_mean, geom="text")+
  scale_y_continuous(name = "Purchases")+
  labs(title = "Средний чек по полу")

# При этом мужчины тратят в среднем больше
fun_mean_cur <- function(x) return(data.frame(y=mean(x), label=paste0(currency(mean(x,na.rm=T), digits = 2), " k")))
ggplot(sample_by_user, aes(Gender, sum_p_thous, fill = Gender))+
  geom_boxplot()+
  stat_summary(fun.data = fun_mean_cur, geom="text")+
  scale_y_continuous(name = "Sum of purchases")+
  labs(title = "Сумма покупок по полу")

# А незамужние тратят больше замужних, причём как женщины, так и мужчины
ggplot(sample_by_user, aes(Gender, sum_p_thous, fill = Gender))+
  geom_boxplot()+
  facet_grid(~Marital_Status)+
  stat_summary(fun.data = fun_mean_cur, geom="text")+
  scale_y_continuous(name = "Sum of purchases")+
  labs(title = "Сумма покупок по полу и семейному положению")

# И средний чек у мужчин больше
ggplot(sample_by_user, aes(Gender, avg_purch_thous, fill = Gender))+
  geom_boxplot()+
  stat_summary(fun.data = fun_mean_cur, geom="text", vjust=-0.2)+
  scale_y_continuous(name = "Sum of purchases")+
  labs(title = "Средний чек по полу")

# Средний чек у замужних женщин больше, чем у незамужних, при этом у мужчин чуть меньше
ggplot(sample_by_user, aes(Gender, avg_purch_thous, col = Gender))+
  geom_boxplot()+
  facet_grid(~Marital_Status)+
  stat_summary(fun.data = fun_mean_cur, geom="text", col = 'black', vjust=-0.2, cex = 2.8)+
  scale_y_continuous(name = "Average check")+
  labs(title = "Средний чек по возрасту статусу и полу")

#здесь делается большой ненужный график
# t <- sample_by_user %>% 
#   group_by(Gender, Age, Marital_Status) %>% 
#   summarise(individual = paste(Age, Gender, sep=" ")[1],
#             group = Marital_Status[1],
#             value = sum(sum_p_thous)/1000) %>% 
#   ungroup() %>% 
#   filter(!is.na(value)) %>% 
#   select(individual, group, value)
# 
# empty_bar <- 1
# 
# to_add <- data.frame(matrix(NA, empty_bar*nlevels(t$group), ncol(t)))
# colnames(to_add) <-  colnames(t)
# to_add$group <- rep(levels(t$group), each=empty_bar)
# t <- rbind(t, to_add)
# t <- t %>% arrange(group)
# t$id <- seq(1, nrow(t))
# 
# label_data <- t
# number_of_bar <- nrow(label_data)
# angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# label_data$hjust<-ifelse( angle < -90, 1, 0)
# label_data$angle<-ifelse(angle < -90, angle+180, angle)
# 
# ggplot(t, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(stat="identity", alpha=0.5) +
#   ylim(max(t$value, na.rm = T)*-0.5,max(t$value, na.rm = T)*2) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank(),
#     plot.margin = unit(rep(-1,4), "cm") 
#   ) +
#   coord_polar() + 
#   geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 


# датасет с брачными и небрачными людьми
diff_married <- sample_by_user %>% 
  group_by(Age, Gender) %>% 
  summarise(diff_p = round(mean(purchases[Marital_Status == "Not Married"]) - 
              mean(purchases[Marital_Status == "Married"])),
            diff_avg_p = mean(avg_purch_thous[Marital_Status == "Not Married"]) - 
              mean(avg_purch_thous[Marital_Status == "Married"]),
            diff_sum_p = mean(sum_p_thous[Marital_Status == "Not Married"]) - 
              mean(sum_p_thous[Marital_Status == "Married"])) %>% 
  filter(!is.nan(diff_sum_p))

# график с разницей между брачными и небрачными людьми
ggplot(diff_married, aes(Age, diff_p, fill = Gender))+
  geom_col(width = 0.4, position = position_dodge(width = 0.5))+
  geom_text(aes(label= diff_p, Age, diff_p+ifelse(diff_p>=0, 0, -0.9)),
            position = position_dodge(width = 0.5),
            vjust = -0.5 , size= 3, color = "black")+
  geom_hline(yintercept = 0)+
  scale_y_continuous(name = "Difference of number purchases")+
  labs(title = "Разница в количестве покупок (не в браке - в браке)")

ggplot(diff_married, aes(Age, diff_avg_p, fill = Gender))+
  geom_col(width = 0.4, position = position_dodge(width = 0.5))+
  geom_text(aes(label= diff_avg_p, Age, diff_avg_p+ifelse(diff_avg_p>=0,0, -0.03)),
            position = position_dodge(width = 0.5),
            vjust = -0.5 , size= 3)+
  geom_hline(yintercept = 0)+
  scale_y_continuous(name = "Difference of average check")+
  labs(title = "Разница в среднем чеке (не в браке - в браке)")

ggplot(diff_married, aes(Age, diff_sum_p, fill = Gender))+
  geom_col(width = 0.4, position = position_dodge(width = 0.7))+
  geom_text(aes(label= diff_sum_p, Age, diff_sum_p+ifelse(diff_sum_p>=0,0, -10)),
            position = position_dodge(width = 0.7),
            vjust = -0.5 , size= 3)+
  geom_hline(yintercept = 0)+
  scale_y_continuous(name = "Difference of sum purchases")+
  labs(title = "Разница в сумме покупок (не в браке - в браке)")


# датасет с группировкой по городам
sample_by_cities <- sample_by_user %>%
  group_by(City_Category, Gender, Age, Marital_Status, Stay_In_Current_City_Years) %>% 
  summarise(users = n(),
            purchases = sum(purchases), 
            sum_p_thous = currency(sum(sum_p_thous), digits = 2),
            avg_purch_thous = sum_p_thous/users,
            avg_purch_per_order = sum_p_thous/purchases) %>% 
  arrange(-sum_p_thous, -avg_purch_thous)

# график распределения расходов по категориям городов
#количество покупок по города
circe_purch <- sample_by_cities %>%
  group_by(City_Category) %>% 
  summarise(purchases = sum(purchases))

ggplot(circe_purch, aes(x = "", purchases, fill = City_Category))+
  geom_bar(stat = "identity", position = position_fill())+
  geom_text(aes(label = purchases), position = position_fill(vjust = 0.5), size = 4)+
  coord_polar(theta = "y")+
  theme(legend.position='bottom')+
  scale_y_continuous(name = "Number of purchases")+
  labs(title = "Количество покупок по типу городов")

#видим, что разница неcущественна
t.test(sapply(levels(sample_by_cities$City_Category), 
              function(x) sum(sample_by_cities$purchases[sample_by_cities$City_Category == x])))

# сумма покупок по городам
circe_purch_sum <- sample_by_cities %>%
  group_by(City_Category) %>% 
  summarise(purchases_sum = sum(sum_p_thous))

ggplot(circe_purch_sum, aes(x = "", purchases_sum, fill = City_Category))+
  geom_bar(stat = "identity", position = position_fill())+
  geom_text(aes(label = purchases_sum), position = position_fill(vjust = 0.5), size = 4)+
  coord_polar(theta = "y")+
  theme(legend.position='bottom')+
  scale_y_continuous(name = "Sum of purchases (thousands)")+
  labs(title = "Сумма покупок по типу городов")

#видим, что разница cущественна
t.test(sapply(levels(sample_by_cities$City_Category), 
              function(x) sum(sample_by_cities$sum_p_thous[sample_by_cities$City_Category == x])))

# средний чек по городам
circe_purch_mean <- sample_by_cities %>%
  group_by(City_Category) %>% 
  summarise(purchases_mean = mean(avg_purch_thous))

ggplot(circe_purch_mean, aes(x = "", purchases_mean, fill = City_Category))+
  geom_bar(stat = "identity", position = position_fill())+
  geom_text(aes(label = purchases_mean), position = position_fill(vjust = 0.5), size = 4)+
  coord_polar(theta = "y")+
  theme(legend.position='bottom')+
  scale_y_continuous(name = "Average check (thousands)")+
  labs(title = "Средний чек по типу городов")

#видим, что разница cущественна
t.test(sapply(levels(sample_by_cities$City_Category), 
              function(x) mean(sample_by_cities$avg_purch_thous[sample_by_cities$City_Category == x])))

#важно делать шапиро тест перед т-тестом
shapiro.test(sapply(levels(sample_by_cities$City_Category), 
                    function(x) mean(sample_by_cities$avg_purch_thous[sample_by_cities$City_Category == x])))

t.test(sapply(levels(sample_by_cities$City_Category), 
              function(x) mean(sample_by_cities$avg_purch_thous[sample_by_cities$City_Category == x])))

users_by_cities <- sample_by_user %>% 
  group_by(City_Category, Stay_In_Current_City_Years) %>% 
  summarise(users = n())

ggplot(users_by_cities, aes(x = Stay_In_Current_City_Years, users, fill = Stay_In_Current_City_Years))+
  geom_col(position = position_dodge(0.6))+
  #stat_summary(fun.y = sum)+
  facet_wrap(~City_Category)+
  geom_text(aes(label = users), position = position_dodge(0.6), size = 4, vjust = -0.5)+
  theme(legend.position='bottom')+
  scale_y_continuous(name = "Жителей")+
  scale_x_discrete(name = "Лет в одном городе")+
  scale_fill_discrete(guide = F)+
  labs(title = "Как долго люди находятся в городах")

# графики по городам
ggplot(sample_by_user, aes(Stay_In_Current_City_Years, avg_purch_thous, col = Stay_In_Current_City_Years))+
  geom_boxplot()+
  #facet_grid(Gender~Marital_Status)+
  stat_summary(fun.data = fun_mean_cur, geom="text", vjust=-0.2, cex = 2.8)+
  scale_y_continuous(name = "Average check")+
  #theme(plot. = element_text(size = 5))+
  labs(title = "Средний чек по количеству лет в городе")
#видим, что разница между средним чеком и годами в городе cущественна
t.test(sapply(levels(sample_by_user$Stay_In_Current_City_Years), 
              function(x) mean(sample_by_user$avg_purch_thous[sample_by_user$Stay_In_Current_City_Years == x])))
#видим, что корреляции нет
cor.test(sample_by_user$avg_purch_thous, 
         as.numeric(sample_by_user$Stay_In_Current_City_Years))

ggplot(sample_by_user, aes(Stay_In_Current_City_Years, purchases, col = Stay_In_Current_City_Years))+
  geom_boxplot()+
  stat_summary(fun.data = fun_mean_cur, geom="text", vjust=0.9, cex = 2.8)+
  scale_y_continuous(name = "Average check")+
  #theme(plot. = element_text(size = 5))+
  labs(title = "Количество покупок по количеству лет в городе")
#видим, что разница cущественна
t.test(sapply(levels(sample_by_user$Stay_In_Current_City_Years), 
              function(x) mean(sample_by_user$purchases[sample_by_user$Stay_In_Current_City_Years == x])))
#видим, что корреляции нет
cor.test(sample_by_user$purchases, 
         as.numeric(sample_by_user$Stay_In_Current_City_Years))

ggplot(sample_by_user, aes(Stay_In_Current_City_Years, sum_p_thous, col = Stay_In_Current_City_Years))+
  geom_boxplot()+
  stat_summary(fun.data = fun_mean_cur, geom="text", vjust=0.9, cex = 2.8)+
  scale_y_continuous(name = "Average check")+
  labs(title = "Сумма покупок по количеству лет в городе")
#видим, что разница cущественна
t.test(sapply(levels(sample_by_user$Stay_In_Current_City_Years), 
              function(x) mean(sample_by_user$sum_p_thous[sample_by_user$Stay_In_Current_City_Years == x])))
#видим, что корреляции нет
cor.test(sample_by_user$sum_p_thous, 
         as.numeric(sample_by_user$Stay_In_Current_City_Years))


#смотрим на покупки по категориям в разрезе лет, проведённых в городе
sample_by_category <- data %>%
  group_by(Product_Category_1, Stay_In_Current_City_Years, City_Category) %>% 
  summarise(purchases = n(), 
            sum_p_thous = currency(sum(Purchase)/1000, digits = 2),
            avg_purch_thous = sum_p_thous/purchases) %>% 
  arrange(-sum_p_thous)

ggplot(sample_by_category, aes(x = Product_Category_1, purchases, fill = Stay_In_Current_City_Years))+
  geom_col(position = position_dodge(0.6))+
  #stat_summary(fun.y = sum)+
  facet_wrap(~City_Category)+
  #geom_text(aes(label = users), position = position_dodge(0.6), size = 4, vjust = -0.5)+
  #coord_polar(theta = "y")+
  theme(legend.position='bottom')+
  scale_y_continuous(name = "Жителей")+
  scale_x_discrete(name = "Лет в одном городе")+
  #scale_fill_discrete(guide = F)+
  labs(title = "Категории в зависимости от лет в городе")

# выявляем самую платёжеспособную аудиторию
ggplot(sample_by_cities, aes(Age, sum_p_thous, fill = Age))+
  geom_col(position = position_dodge(0.6))+
  facet_grid(City_Category~Gender+Marital_Status)+
  #stat_summary(fun.data = fun_mean_cur, geom="text", hjust = -0.5, vjust = 0.5, position = position_dodge(0.6), col = 'black', angle = 90,  size = 2.5)+
  theme(legend.position='bottom')+
  scale_x_discrete(name = "Возраст")+
  scale_y_continuous(name = "Сумма покупок", breaks = seq(0, 1.5e05, by = 2e4))+
  scale_fill_discrete(guide = F)+
  labs(title = "Наиболее платёжеспособная аудитория")

final_graph <- sample_by_cities %>% 
  group_by(Marital_Status, Gender, Age, City_Category) %>%
  mutate(users = paste(Marital_Status, Gender, Age, City_Category, sep = ", ")) %>%
  summarise(users = users[1], 
            sum_p_thous = sum(sum_p_thous), 
            avg_purch_per_order = mean(avg_purch_per_order)) %>% 
  ungroup() %>% 
  select(users, sum_p_thous, avg_purch_per_order)

ggplot(final_graph, aes(x = reorder(final_graph$users, final_graph$sum_p_thous,
                                    function(x) -max(x)), y = sum_p_thous, fill = users))+
  geom_col(position = "identity")+
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  scale_x_discrete(name = "Тип пользователей")+
  scale_y_continuous(name = "Сумма покупок", breaks = seq(0, 1.5e06, by = 2e4))+
  scale_fill_discrete(guide = F)+
  labs(title = "Наиболее платёжеспособная аудитория")

##сделать логистическую регрессию по браку и типу города (растёт ли или убывает)
