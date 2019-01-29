bad_var_estimator <- function(x){
  n <- length(x)
  var(x)*(n-1)/n
}

JN_bias_correct <- function(x, estimator){
  n <- length(x)
  theta_stars <- vector("numeric", n)
  
  ind <- 1:n
  
  for (i in ind){
    sample <- x[ind != i]
    theta_stars[i] <- estimator(sample)
  }
  
  theta_hat <- estimator(x)
  theta_dot <- mean(theta_stars)
  
  bias_jack <- (theta_dot - theta_hat)*(n - 1)
  
  theta_hat_jack <- theta_hat - bias_jack
  
  theta_hat_jack
}

x <- bad_var_estimator(sample(1:200, size = 100))
JN_bias_correct(sample(1:200, size = 100), bad_var_estimator)

smpl <- sample(1:200, size = 100)
var(smpl)

bad_var_estimator(smpl)
JN_bias_correct(smpl, bad_var_estimator)


#дз1
median_cl_boot <- function(x){
  x_med <- median(x)
  samples <- replicate(1000, median(sample(x, size = length(x), replace = T)))
  samples
  differences <- samples - x_med
  quantile(differences, probs = c(0.025, 0.975)) + x_med
}

median_cl_boot2 <- function(x){
  boot_med <- replicate(1000,median(sample(x,length(x),replace = T)))
  median_x <- median(x)
  median_x+quantile(boot_med-median_x, probs = c(0.025, 0.975))
}

x <- sample(1:1000, size = 100)
median_cl_boot(x)
median_cl_boot2(x)


#дз2
slope_cl_boot <- function(x){
  replications <- 1000
  cor <- lm(y ~ x, data = x)$coefficients[2]
  samples <- replicate(replications, lm(y ~ x, x[sample(nrow(x), replace=T),])$coefficients[2] - cor)
  cor + quantile(samples, c(.025,.975))
}


slope_cl_boot(mtcars[,c(1:2)])
lm(mpg ~ disp, mtcars)$coefficients[2]
