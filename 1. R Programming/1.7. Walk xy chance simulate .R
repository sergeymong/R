# Random walk with absorption
simulate_walk <- function(r = 6, x = 0, y = 0, n_max = 100, p = 0.01) {
  current_calc <- function(x, y) return(sqrt(x^2 + y^2))
  for (i in 1:n_max) {
    is_absorbed <- rbinom(1, 1, p)
    if (is_absorbed) return (1)
    x <- x + rnorm(1)
    y <- y + rnorm(1)
    if (current_calc(x, y) > r) return (2)
  }
  return (3)
}

# Simulate results
num_of_calls <- 100000
result <- replicate(num_of_calls, simulate_walk(), simplify = T)
chance_br <- length(which(result == 2))/num_of_calls


