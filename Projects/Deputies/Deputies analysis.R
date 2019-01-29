library(readxl)
library(dplyr)
library(formattable)

data <- read_xlsx('Государственная_Дума_Антикоррупционная.xlsx', skip = 1)

mans <- sapply(data[1], function(x) grepl('.*ьич', x))
mans <- sapply(data[1], function(x) grepl('.*вич', x))
mans <- sapply(data[1], function(x) grepl('.*вич|.*ьич', x))
womans <- sapply(data[1], function(x) grepl('.*вна', x))
data$gender <- ifelse(sapply(data[1], function(x) grepl('.*вич|.*ьич', x)), 'man', 'woman')

data$names <- NA
names_w <- strsplit(data[womans,][[1]], '([[:punct:]]*вна)', perl = T)
names_w <- paste0(sapply(names_w, function(x) x[1]), 'вна')
names_m <- strsplit(data[mans,][[1]], '([[:punct:]]*вич)', perl = T)
names_m <- paste0(sapply(names_m, function(x) x[1]), 'вич')
data[mans,]$names <- gsub('\n', ' ', names_m)


partys <- strsplit(data[[1]], 'Партия: ([[:punct:]]*)', perl = F)
partys <- sapply(partys, function(x) x[2])
partys <- gsub('\nПоказать подробности', '',partys)
data$party <- partys


by_gender <- data %>% 
  group_by(gender) %>% 
  summarise(Доход_med = currency(median(`Доход, руб.`), 'RUB ', digits = 0),
                  Доход_mean = currency(mean(`Доход, руб.`), 'RUB ', digits = 0),
                  Доход_min = currency(min(`Доход, руб.`), 'RUB ', digits = 0),
                  Доход_max = currency(max(`Доход, руб.`), 'RUB ', digits = 0),
                  Доход_sum =  currency(sum(`Доход, руб.`), 'RUB ', digits = 0),
                  Количество = n())

by_party <- data %>% 
  group_by(party) %>% 
  summarise(Доход_med = currency(median(`Доход, руб.`), 'RUB ', digits = 0),
                  Доход_mean = currency(mean(`Доход, руб.`), 'RUB ', digits = 0),
                  Доход_min = currency(min(`Доход, руб.`), 'RUB ', digits = 0),
                  Доход_max = currency(max(`Доход, руб.`), 'RUB ', digits = 0),
                  Доход_sum =  currency(sum(`Доход, руб.`), 'RUB ', digits = 0),
                  Количество = n())


