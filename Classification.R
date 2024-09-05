library(mvtnorm)

###Real 
library(klaR)
library(psych)
library(MASS)
#library(ggord)
library(devtools)
data("iris")
str(iris)
pairs.panels(iris[1:4],
             gap = 0,
             bg = c("red", "green", "blue")[iris$Species],
             pch = 21)

set.seed(123)
ind <- sample(2, nrow(iris),
              replace = TRUE,
              prob = c(0.6, 0.4))
training <- iris[ind==1,]
testing <- iris[ind==2,]

linear <- lda(Species~., training)
linear
partimat(Species~., data = training, method = "lda")

p1 <- predict(linear, training)$class
tab <- table(Predicted = p1, Actual = training$Species)
tab

p2 <- predict(linear, testing)$class
tab1 <- table(Predicted = p2, Actual = testing$Species)
tab1

sum(diag(tab1))/sum(tab1)

## PCA
library(ade4)
data(deug)
a=deug$tab
ev=eigen((cov(a)))$values
eve=eigen(cov(a))$vectors
l1=eve[1:9,1]%*%t(a)
l2=eve[1:9,2]%*%t(a)
plot(-l1[1,1:104], l2[1,1:104])
plot(sort(-l1[1,1:104]))
