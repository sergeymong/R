library(ggplot2)
d <- iris[, c("Sepal.Length", "Petal.Width")]

fit <- kmeans(d, 3)
d$clusters <- factor(fit$cluster)

ggplot(d, aes(Sepal.Length, Petal.Width, col = clusters))+
  geom_point(size = 2)+
  theme_bw() 


df <- data.frame(x = c(-3, 1, 2, 3, 5, 6, 7), y = c(3, 4, 6, 8, 2, 11, 1))


kmeans(df, 1)$tot.withinss == sum(apply(df, 1, function(x) (x[1]^2 - 3^2) + (x[2]^2 - 5^2)))


sqrt((3+3)^2 + (3-5)^2) 

means <- apply(df, 1, function(x) sqrt((x[1] - 3)^2 + (x[2] - 5)^2))


#ирерахическая кластеризация
library(ggplot2) 
library(ggrepel) # для симпатичной подписи точек на графике

x <- rnorm(10)
y <- rnorm(10)
test_data <- data.frame(x, y)
test_data$labels <- 1:10

ggplot(test_data, aes(x, y, label = labels))+
  geom_point()+
  geom_text_repel()

d = dist(test_data)
fit <- hclust(d, method = "single")
plot(fit, labels = test_data$labels)
rect.hclust(fit, 3) # укажите желаемое число кластеров, сейчас стоит 2

library(ape)
set.seed(222)
tr <- rtree(20, tip.label = c("B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U")) 
#левое дерево
plot.phylo(tr) 
#правое дерево 
plot.phylo(tr, use.edge.length=T)


install.packages("pca3d")
library(pca3d)

dt <- swiss
dt$is_catholic <- ifelse(swiss$Catholic > 50, 1, 0)
dt$is_catholic <- factor(dt$is_catholic)
fit <- prcomp(swiss, center = T)
pca3d(fit, group = dt$is_catholic,
      fancy = T, 
      new=T)

# практика 
# дз1
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
smart_hclust<-  function(test_data, cluster_number){
  dist_matrix <- dist(test_data)
  fit <- hclust(dist_matrix)
  test_data$cluster <- as.factor(cutree(fit, cluster_number))
  test_data
}


#дз2
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")

get_difference<-  function(test_data, n_cluster){
  formula <- sapply(names(test_data), function(x) paste(x, "~ cluster"))
  test_data$cluster <- as.factor(cutree(hclust(dist(test_data)), n_cluster))
  names(which(sapply(formula, function(x) summary(aov(as.formula(x), test_data))[[1]]$`Pr(>F)`[1]) < 0.05))
}

get_difference_2 <- function(test_data, n_cluster){    
  dist_matrix <- dist(test_data)    
  fit <- hclust(dist_matrix)    
  test_data$cluster <- as.factor(cutree(fit, n_cluster))    
  p_val <- sapply(test_data[,-ncol(test_data)],    
                  function(x) {summary(aov(x~cluster, test_data))[[1]][1,'Pr(>F)']})    
  return(names(p_val)[p_val < 0.05])    
}

get_difference_3 <- function(df, n_clusters){
  fit <- hclust(dist(df))
  cluster <- factor(cutree(fit, n_clusters))
  is.good <- sapply(df, function(x) anova(aov(x ~ cluster))$P[1] < 0.05)
  names(df)[is.good]
}

#дз3
test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
get_pc <- function(d){
  components <- prcomp(d)
  d$PC1 <- components$x[,1]
  d$PC2 = components$x[,2]
  d
}

get_pc2 <- function(test){    
  fit <- prcomp(test)    
  test<- cbind(test, fit$x[,1:2])    
  return(test)    
}

#дз4
get_pca2 <- function(d){
  components <- prcomp(d)
  nums <- which.max(summary(components)$importance[3,] >= 0.9)
  d <- cbind(d, components$x[,1:nums])
  d
}
get_pca2(swiss)

#дз5
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_1.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")
test_data <- as.data.frame(list(V1 = c(13, 30, 12, 14, 13), V2 = c(-3, 15, 22, 14, 21), V3 = c(-4, -10, -4, -12, -1), V4 = c(9, 15, 9, 17, 6), V5 = c(4, 3, 17, 7, 8), V6 = c(-8, 10, 17, 9, 16)))

is_multicol <- function(d){
  diff_mat <- abs(as.data.frame(apply(d, 2, diff)))
  multicol <- as.matrix(sapply(1:ncol(diff_mat), function(x) sapply(diff_mat, identical, diff_mat[,x])))
  nms <- names(which(apply(multicol, 1, sum) == 2))
  if(length(nms) == 0) "There is no collinearity in the data" else nms
}

is_multicol2 <- function(d){    
  d <- abs(cor(d))     
  d[lower.tri(d)] <- 0    
  diag(d) <- 0    
  index <- which((1-d) < 1e-10, arr.ind = T)   
  index
  if (length(index) == 0){
    return('There is no collinearity in the data')
  } else {
    return(rownames(d)[index])
  }
}

#дз6
swiss <- smart_hclust(swiss, 2)
ggplot(swiss, aes(Education, Catholic, col = cluster))+
  geom_point()+
  geom_smooth(method = "lm")
