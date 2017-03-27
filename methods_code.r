

# 1. KNN

> set.seed(123)
> dim(iris)
[1] 150   5
> n <- sort(c(sample(1:50, 35), sample(51:100, 35), sample(101:150, 35)))
> train_x <- iris[n,1:4]
> train_y <- iris[n,5]
> distance <- function(a, b) return(sqrt((a[1]-b[1])^2+(a[2]-b[2])^2+(a[3]-b[3])^2+(a[4]-b[4])^2))
