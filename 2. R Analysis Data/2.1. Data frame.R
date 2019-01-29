# Working with subsets and datasets
data <- read.csv("./Datasets/1080p.csv")
data[data$gender == "female", 1:3]
mean(data[data$gender == "male", 1]) 

mtcars$even_gear <- ifelse (mtcars$gear %% 2 == 0, 1, 0)
mpg_5 <- subset(mtcars$mpg, mtcars$cyl==4)
mini_mtcars <- subset(mtcars[c(3, 7, 10, 12, nrow(mtcars)),])
mtcars$new_var <- ifelse(mtcars$carb >= 4 | mtcars$cyl <= 6, 1, 0)
if (mean(my_vector) > 20) print("My mean is great") else "My mean is not so great"


str(AirPassengers)

#изящное векторизованное решение по поиску только тех значений, которые больше предыдущих
good_months <- AirPassengers[-1][AirPassengers[-1] > AirPassengers[-144]] 

airp <- c(AirPassengers)
moving_average <- sapply(1:(length(AirPassengers)-9), function(x) mean(AirPassengers[x:(x+9)]))
moving_average

