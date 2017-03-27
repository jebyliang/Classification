

# 1. KNN

distance <- function(a, b) return(sqrt((a[1]-b[1])^2+(a[2]-b[2])^2+(a[3]-b[3])^2+(a[4]-b[4])^2))

library(dplyr)
iris_knn <- function(x, k){
  n <- sort(sample(1:dim(iris)[1], .7*dim(iris)[1]))
  train_x <- as.matrix(iris[n,1:4])
  train_y <- data.frame(iris[n,5])
  colnames(train_y) <- names(iris)[5]
  train_y$dis <- NULL
  for (i in dim(train_x)[1]){
    train_y$dis[i] <- distance(as.vector(x), as.vector(train_x[i,]))
  }
  train_y <- arrange(train_y, dis)[1:k,] %>% group_by(Species) %>% count(Species) %>% arrange(desc(n))
  return(as.character(train_y$Species[1]))
}

