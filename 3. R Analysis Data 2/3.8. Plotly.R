smoothed.vector <- fitted(loess(uempmed ~ as.numeric(date), economics, span = 0.2))
my.plot <- ggplot(economics) + 
  geom_line(aes(x = ~date, y = ~uempmed), color = "red") + 
  geom_line(aes(x = ~date, y = ~smoothed.vector), color = "blue")
ggplotly(my.plot)



points <- data.table(
  x = c(0.2, 0.8, 0, 1),
  y = c(0, 0, 1, 1),
  z = c(0, 0, 0, 0)
)

i.s <- c(0, 2)
j.s <- c(1, 1)
k.s <- c(2, 3)
plot_ly(points, x = ~x, y = ~y, z = ~z, i = ~i.s, j = ~j.s, k = ~k.s, type = "mesh3d")


smoothed.vector <- fitted(loess(uempmed ~ as.numeric(date), economics, span = 0.2))
plot_ly(economics, x = ~date, y = ~uempmed, type = "scatter", showlegend = FALSE, mode="lines+markers") %>%
  add_trace(x = date, y = smoothed.vector)

smoothed.vector <- fitted(loess(uempmed ~ as.numeric(date), economics, span = 0.2))
my.plot <- ggplot(economics) + 
  geom_line(aes(x = date, y = uempmed), color = "red") + 
  geom_line(aes(x = date, y = smoothed.vector), color = "blue")
ggplotly(my.plot)

plot_ly(z = ~volcano, type="surface")

#домашнее задание -- 3D чайник!
teapot <- read.csv("./Datasets/teapot.csv", sep = ';')
make.fancy.teapot <- function(teapot.coords) {
  ind <- data.table(i = seq(0, nrow(teapot.coords)-1, by = 3),
                    j = seq(1, nrow(teapot.coords)-1, by = 3),
                    k = seq(2, nrow(teapot.coords)-1, by = 3))
  plot_ly(teapot.coords, x = ~teapot.coords$x, 
          y = ~teapot.coords$y, 
          z = ~teapot.coords$z, 
          i = ~ind$i, 
          j = ~ind$j, 
          k = ~ind$k, 
          type = "mesh3d")
}

make.fancy.teapot(teapot)

head(teapot[seq(3, nrow(teapot), by = 3),])
seq(1, nrow(teapot), by = 3)
seq(2, nrow(teapot), by = 3)
