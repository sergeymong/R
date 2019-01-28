# Код, написанный в процессе просмотра урока и решения заданий
bind_diag <- function(m1, m2, fill) {
  m3 <- matrix(fill, 
               nrow = nrow(m1) + nrow(m2),
               ncol = ncol(m1) + ncol(m2))
  m3[1:nrow(m1), 1:ncol(m1)] <- m1
  m3[nrow(m1) + 1:nrow(m2), ncol(m1) + 1:ncol(m2)] <- m2
  m3
}

# проверка функции
m1 <- matrix(1:12, nrow = 3)
x <- col(m1[])
m2 <- matrix(10:15, ncol = 3)
bind_diag(m1, m2, fill = NA)


# ДЗ. Моя функция
build_ziggurat <- function(n) {
  # Моя первая функция в R. Постройка зиккурата.
  # На входе функции n -- число, которое обозначает количество слоёв.
  # Например:
  # > build_ziggurat(3)
  #       [,1] [,2] [,3] [,4] [,5]
  # [1,]    1    1    1    1    1
  # [2,]    1    2    2    2    1
  # [3,]    1    2    3    2    1
  # [4,]    1    2    2    2    1
  # [5,]    1    1    1    1    1
  
  maxi <- n-1
  m <- matrix(NA, nrow = 1, ncol = n*2 - 1)
  if (n > 1) av <- c(1:n, (n-1):1) else av <- c(1)
  m <- matrix(av, nrow = 1)
  while(maxi > 0) {
    m1 <- matrix(av, nrow = 1)
    m2 <- m1
    m2[which(max(av)==av)] <- m2[which(max(av)==av)]-1
    m <- rbind(m2, m, m2)
    av <- c(m2)
    maxi <- maxi - 1
  }
  return (m)
}

# функции других участников для бенчмаркинга
build_ziggurat2 <- function(n){
  out <- mat.or.vec(nr = (n - 1)*2 + 1, nc = (n - 1)*2 + 1) + 1
  for(i in seq(n - 1)){
    out[seq(i + 1, nrow(out) - i), seq(i + 1, ncol(out) - i)] <- i + 1
  }
  return(out)
}
build_ziggurat3 <- function(n, level = 1) {
  m <- matrix(level, nrow = n * 2 - 1, ncol = n * 2 - 1)
  if (n > 1) m[2:((n-1) * 2),2:((n-1) * 2)] <- build_ziggurat(n-1, level + 1)
  return(m)
}
build_ziggurat4 <- function(n) {
  d <- n * 2 - 1
  m <- matrix(0, d, d)
  for (i in 1:n) {
    i2 <- d - i + 1
    m[i:i2, i:i2] <- i
  }
  m
}
build_ziggurat5 <- function(n) {
  d <- n * 2 - 1
  outer(1:d, 1:d, function(x,y) {
    x <- n - abs(n - x)
    y <- n - abs(n - y)
    pmin(x,y)
  })
}
build_ziggurat6 <- function(n) {
  size <- n *2-1
  temp <- matrix(NaN, size, size)
  pmin(n-abs(n - row(temp)), n-abs(n-col(temp)))
}
build_ziggurat7 <- function(n) {
  if (n == 1) {return(matrix(1,1,1))}
  
  
  k <- matrix(0,2*n-1,2*n-1)
  k[n,n] <- n
  
  for (i in 1:(n-1)) {
    k[n+i,n+i] <-  n-i
    k[n-i,n-i] <-  n-i
    k[n-i,n+i] <-  n-i
    k[n+i,n-i] <-  n-i
  }
  
  for (j in 1:(2*n-1)) {
    for (i in j:(2*n-j)) {
      k[j,i]<-k[j,j]
      k[i,j]<-k[j,j]
    }
  }
  return(k)
}
build_ziggurat8 <- function(n) {
  k <-1
  e <- 2*n-1
  l <- matrix(0,2*n-1,2*n-1)
  while (n >= 1) {
    m <- matrix(k,2*n-1,2*n-1)
    l [k:e,k:e]<-m
    k <- k+1
    n <- n-1
    e <- e-1
  }
  return(l)
}
build_ziggurat9 <- function(n) {
  d = n*2-1
  # создадим 4 ступенчатые матрицы, направленные в разные стороны
  m1 = matrix(1:d, d, d)
  m2 = matrix(1:d, d, d, byrow = T)
  m3 = matrix(d:1, d, d)
  m4 = matrix(d:1, d, d, byrow = T)
  print("m1 is")
  print(m1)
  print("m2 is")
  print(m2)
  print("m3 is")
  print(m3)
  print("m4 is")
  print(m4)
  
  # найдем минимум для каждой позиции из четырех матриц
  pmin(m1, m2, m3, m4)
}


# проверка скорости функций при построении большого зиккурата
benchmark(build_ziggurat(100))
benchmark(build_ziggurat2(100))
benchmark(build_ziggurat3(100))
benchmark(build_ziggurat4(100))
benchmark(build_ziggurat5(100))
benchmark(build_ziggurat6(100))
benchmark(build_ziggurat7(100))
benchmark(build_ziggurat8(100))
benchmark(build_ziggurat9(100))


# Код, написанный в процессе просмотра урока и решения заданий. Работа с листами.
list(a = list(1, 2, 3), b = list(list(4)), 5, 6)

l1 <- list(name = "John", salary = 1000)
l2 <- list(has_car = TRUE, car = "lamborghini")
l3 <- c(l1, l2)
