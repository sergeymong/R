count_elements75256847 <- function(x) {
  l1 <- unique(sort(x))
  l2 <- rle(sort(x))$lengths
  rbind(l1, l2)
}

count_elements15795006 <- function(x) {
  t <- table(x)
  rbind(as.numeric(names(t)), t)
}

count_elements15794973 <- function(x) sapply(sort(unique(x)), function (n) c(n, sum(x == n)))

count_elements21908203 <- function(x) {
  v <- sort(unique(x))
  rbind(v, sapply(v, function(i) sum(x == i)))
}

count_elements16274902 <- function(x) {
  return(t(as.data.frame(table(x))))
}

count_elements65079537 <- function(x) {
  m <- matrix(NA, ncol = length(unique(x)) ,nrow = 2)
  m[1,] = sort(unique(x))
  res <- c()
  for (i in m[1,]) {
    res <- c(res,sum(i == x))
  }
  m[2,] <- res
  m
}

count_elements21014906 <- function(x) {
  v <- sort(unique(x))
  w <- sapply(v, function(a) length(x[x == a]))
  matrix(c(v, w), nrow = 2, byrow = T)
}

x <- sample(1:10e10, 10e3, replace = TRUE)

benchmark(count_elements75256847(x))
benchmark(count_elements15795006(x))
benchmark(count_elements15794973(x))
benchmark(count_elements21908203(x))
benchmark(count_elements16274902(x))
benchmark(count_elements65079537(x))
benchmark(count_elements21014906(x))
