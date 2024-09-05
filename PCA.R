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

## contribution ratio
Cr=rep(0,9);
for (k in 1:9)
{
  Cr[k]=sum(ev[1:k])/sum(ev)
}

plot(Cr, xlab = "number",pch = 19,col="blue")
abline(h=0.8, col="red")

## overall threshold
plot(ev, xlab = "number",pch = 19,col="blue")
abline(h=mean(ev), col="red")

## Scree

plot(ev, xlab = "number",pch = 19,col="blue")
plot(ev[1:8]-ev[2:9], xlab = "number", ylab="difference", pch = 19,col="blue")

## hypothesis testing

u=rep(0,8)
for (k in 1:8)
{
  inte_mediate=1/(9-k+1)*sum(ev[k:9])
  u[k]=(104-(2*9+11)/6)*((9-k+1)*log(inte_mediate)-sum(log(ev[k:9])))
}

######Case study

library("ggplot2")
library("ggfortify")
library("gridExtra")
library("carData")
library("car")
library("factoextra")
library("corrplot")
data(decathlon2)
decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active)
summary(decathlon2.active)
res.pca <- prcomp(decathlon2.active, scale = TRUE)
print(res.pca)
summary(res.pca)
eig.val<-get_eigenvalue(res.pca)
eig.val
#fviz_eig(res.pca, col.var="blue")
plot(eig.val$eigenvalue, ylab = "eigenvalue", xlab = "number",pch = 19,col="blue")
abline(h=mean(eig.val$eigenvalue),col="red")

var <- get_pca_var(res.pca)
var

#head(res.pca$cos2,4)

> a=res.pca$x
> plot(a[1:23,1],a[1:23,2])
> abline(h=0)
> abline(v=0)
> a=res.pca$x
> b=a[1:23,1]*2+1.35*a[1:23,2]
