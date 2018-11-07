rm(list = ls())
setwd("/Users/juc1987/prog/progr")
source("./sources/numerico.R")


fp <- function(t,y){
  2*t*y
}


h <- 0.1
x <- seq(1,1.5,h)
y <- rep(0,length(x))
y[1] <- 1
for (i in 2:length(y)) {
  k1 <- h*fp(x[i-1],y[i-1])
  k2 <- h*fp(x[i-1] + h, y[i-1] + k1)
  y[i] <- y[i-1] + (k1+k2)/2
}
puntos <- cbind(x,y)

sol <- newton(puntos)
xgra <- seq(1,1.5,0.01)
f <- function(x) evalpoli(x,sol)
ygra <- sapply(xgra, f)

par(mfrow=c(1,2))
plot(xgra,ygra, type = "l")
points(x,y, col="red")
freal <- function(x) 2.1782^(x^2)/2.1782
xreal <- xgra
yreal <- sapply(xreal, freal)
plot(xreal,yreal,type = "l")
points(x,y,col="red")

par(mfrow = c(1,1))
plot(xgra,ygra,type = "l",col = "royalblue")
lines(xreal,yreal,type = "l",col = "red")
