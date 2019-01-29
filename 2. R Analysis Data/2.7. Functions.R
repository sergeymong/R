my_calc <- function(x, y){
  s <- x + y
  s
}
my_calc(1,3)

distr1 <- rnorm(100)
distr1[1:30] <- NA

distr1[is.na(distr1)] <- mean(distr1, na.rm = T)
my_na_rm <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = T)
  x
}
my_na_rm(distr1)

#домашнее задание 1
NA.position <- function(x){
  return(sum(is.na(x)))
}

sum(is.na(distr1))

#домашнее задание 2
filtered.sum <- function(x){
  without_na <- x[-which(is.na(x))]
  return(sum(without_na[without_na > 0], na.rm = T))
}


# домашнее задание 3
outliers.rm <- function(x){
  quant <- quantile(x, probs = c(0.25, 0.75))
  return(x[(quant[1]-1.5*IQR(x)) < x & (quant[2]+1.5*IQR(x))>x])
}

x <- c(11.38, -0.92, -1.36, -0.27, 0.6, 0.36, 0.37, 1.01, 1.19, 0.22, -2.44, 27.37, 0.86, 0.09, 2.93, 0.12, 0, -0.16, -2.46, 0.04, 0.92, -1.67, -0.25, 0.49, -21.87, 15.25, 0.07, 106.38, 0.92, 0.51)
outliers.rm(x)

quant <- quantile(x, probs = c(0.25, 0.75))

x - which(x[(quant[1]-1.5*IQR(x))<x]) - x[(quant[2]+1.5*IQR(x))>x]


quant[1]-(1.5*quant[1])
x[(quant[1]-1.5*quant[1])<x]
x[(quant[2]+1.5*quant[1])>x]
