

# 1. KNN
set.seed(1)
training_rows <- sort(c(sample(1:50, 40), sample(51:100, 40), sample(101:150, 40)))
training_x <- as.matrix(iris[training_rows, 1:4])
training_y <- iris[training_rows, 5]
iris_new <- iris[training_rows, ]

library(dplyr)
distance <- function(a, b){
  return (sqrt((a[1]-b[1])^2+(a[2]-b[2])^2+(a[3]-b[3])^2+(a[4]-b[4])^2))
}

iris_knn <- function(x, trainx, trainy, k){
  trainy <- data.frame(trainy)
  colnames(trainy) <- "Species"
  trainy$dis <- NULL
  for (i in 1:dim(trainx)[1]){
    trainy$dis[i] <- distance(as.vector(x), as.vector(trainx[i,]))
  }
  trainy <- arrange(trainy, dis)
  # if distance ties, expand the k
  while(trainy$dis[k] == trainy$dis[k+1]){
    k <- k+1
  }
  new_trainy <- trainy[1:k, ] %>% group_by(Species) %>% count(Species) %>% arrange(desc(n))
  # if 3 species within k
  if (dim(new_trainy)[1] == 3){
    if (new_trainy$n[1] == new_trainy$n[2] & new_trainy$n[1] == new_trainy$n[3]){
      # if all species tie, choose the closest one other than itself
      result <- trainy$Species[2] 
    } else if (new_trainy$n[1] == new_trainy$n[2] & new_trainy$n[1] != new_trainy$n[3]){
      # if first two species ties, choose the cloeset distance between two species
      trainy <- trainy[-which(trainy$Species == new_trainy$Species[3]),]
      result <- trainy$Species[2] 
    }
    # if 2 species within k
  } else if (dim(new_trainy)[1] == 2){
    trainy <- trainy[which(trainy$Species == new_trainy$Species[1] | trainy$Species == new_trainy$Species[2]), ] # exclude the 3rd specy
    if (new_trainy$n[1] == new_trainy$n[2]){
      # if 2 species ties, output the closest one
      result <- trainy$Species[2] 
    } else {
      # otherwise output the most species between them
      result <- new_trainy$Species[1]
    }
  } else result <- new_trainy$Species # if only 1 specy within k, output this specy
  return(as.character(result))
}

set.seed(100)
test_point <- NULL
for (i in 1:4){
  test_point <- append(test_point, runif(1, min(iris[,i]), max(iris[,i])))
}
test_point
iris_knn(test_point, training_x, training_y, 5)

iris_new <- iris
iris_new <- rbind(iris_new, append(test_point, NA))

iris_new$Species <- factor(iris_new[,5], levels=c(levels(iris_new[,5]), paste(iris_knn(test_point, training_x, training_y, 5), "test")))
iris_new$Species[151] <- paste(iris_knn(test_point, training_x, training_y, 5), "test")

plot_ly(iris_new, x = ~Sepal.Length, y = ~Sepal.Width, z = ~Petal.Length, color = ~Species)
  


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

