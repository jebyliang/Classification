

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
  
# 2. Kmeans
  
# data preparing
set.seed(123)
library(mvtnorm)
cv <- matrix(c(1,.3,.3,1), ncol=2)
j <- rmvnorm(100, mean = c(3,3), sigma = cv)
k <- rmvnorm(100, mean = c(5,8), sigma = cv)
l <- rmvnorm(100, mean = c(8,3), sigma = cv)
dat <- rbind(j,k,l)
true_groups <- as.factor(c(rep(1,100),rep(2,100),rep(3,100) ))
# randomly assign 3 three groups as initial groups
guess <- factor(sample(c(1,2,3), 300, replace = TRUE))
plot(dat, col = guess, xlab = "X", ylab = "Y")

distance <- function(x, y) return(sqrt((x[1] - y[1])^2 + (x[2] - y[2])^2))
classifier <- function(p, centroids) return(which.min(c(distance(p, centroids[1,]), distance(p, centroids[2,]), distance(p, centroids[3,]))))

centroids <- 0
j_locat <- NULL
k_locat <- NULL
l_locat <- NULL
while(sum(centroids - aggregate(dat ~ guess, FUN = mean)[1:3,2:3]) != 0){
  centroids <- aggregate(dat ~ guess, FUN = mean)[1:3,2:3]
  j_locat <- rbind(j_locat, centroids[1,])
  k_locat <- rbind(k_locat, centroids[2,])
  l_locat <- rbind(l_locat, centroids[3,])
  groups <- NULL
  for (i in 1:dim(dat)[1]) groups <- append(groups, classifier(dat[i,], centroids))
  guess <- factor(groups)
}

plot(dat, col = guess, xlab = "X", ylab = "Y")
lines(j_locat[,1], j_locat[,2], type = "o", col = 1, pch = 16, lty = 1)
lines(k_locat[,1], k_locat[,2], type = "o", col = 2, pch = 16, lty = 1)
lines(l_locat[,1], j_locat[,2], type = "o", col = 3, pch = 16, lty = 1)
table(guess, true_groups)
     true_groups
guess   1   2   3
    1 100   0   1
    2   0 100   0
    3   0   0  99

