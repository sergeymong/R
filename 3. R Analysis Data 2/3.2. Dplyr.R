my_data <- data_frame(x = rnorm(10000), y = rnorm(10000), f = factor(rep(1:2, 5000)))

diamonds <- as_data_frame(diamonds)

select(diamonds, cut, price)

slice(diamonds, premium)

t <- mutate(diamonds, new_price = sqrt(price))

# домашняя работа 1
d <- slice(diamonds, seq(1, nrow(diamonds), 2))

# домашняя работа 2
mtcars %>% 
  filter(mpg > 14, hp > 100) %>%
  arrange(desc(mpg)) %>%
  slice(1:10) %>%
  select(mpg, hp, am, vs) %>% 
  rename("Miles per gallon" = mpg, "Gross horsepower" = hp)

#домашняя работа 3
all_to_factor <- function(x){
  x %>% 
    mutate_each(funs(as.factor(.)))
}
  
# домашняя работа 4
log_transform <- function(test_data){
  t <- test_data %>% 
    mutate_if(is.numeric, function (x) x = log((x - min(x))/(max(x) - min(x)) + 1))
  t
}

#используем подмену переменных из dplyr
log_transform <- function(test_data){
  test_data %>% mutate_if(is.numeric, funs(log((.-min(.))/(max(.)-min(.))+1)))
  
}

#проверяем домашнее задание
test_data <- data_frame(V1 = c(1.5, -0.1, 2.5, -0.3, -0.8), V2 = c(-0.9, -0.3, -2.4, 0.0, 0.4), V3 = c(-2.8, -3.1, -1.8, 2.1, 1.9), V4 = c("A", "B", "B", "B", "B"))
log_transform(test_data)


# продолжение классной работы
diamonds <- as_data_frame(diamonds)

gr_diam <- group_by(diamonds, cut)

by_species <- iris %>% group_by(Species)
by_species %>% summarise_at(vars(Petal.Width), length)

select_if(iris, is.numeric) %>% 
  summarise_all(funs(sd))


# домашняя работа 5
descriptive_stats <- function (dataset){
  dataset %>% 
    group_by_(.dots = names(which(sapply(dataset, is.factor)))) %>% 
    select_if(is.numeric) %>% 
    summarise_all(funs(
      n = n(),
      mean = mean(., na.rm = T),
      sd = sd(., na.rm = T),
      median = median(., na.rm = T),
      first_quartile = quantile(., 0.25, na.rm = T),
      third_quartile = quantile(., 0.75, na.rm = T),
      na_values = sum(is.na(.))
    ))
}

# проверка дз5
test_data <- as_data_frame(read.csv("https://stepic.org/media/attachments/course/724/salary.csv"))
diam_des <- descriptive_stats(diamonds)

# домашняя работа 6
to_factors <- function(test_data, factors){
  test_data %>% 
    mutate_at(factors, funs(
      as.factor(ifelse(. > mean(.), 1, 0))
      ))
}
# проверка дз6
to_factors(mtcars[1:4], factors = c(1, 3))

# домашняя работа 7

fun1 <- function(){
  diamonds %>% 
    select(color, price) %>% 
    group_by(color) %>% 
    arrange(desc(price)) %>%
    slice(1:10)
}

fun2 <- function() {
  diamonds %>% group_by(color) %>%     
    arrange(desc(price)) %>% slice(1:10) %>% select(color, price)
}

fun1()
fun2()
benchmark(fun1(), replications = 1000)
benchmark(fun2(), replications = 1000)



