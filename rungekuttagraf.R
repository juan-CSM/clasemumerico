rm(list = ls())
setwd("/Users/juc1987/prog/progr")
source("./sources/numerico.R")


fp <- function(t,y){
  2*t*y
}

h <- 0.1
desde <- c(1,1.5)
y0 <- 1

resultado <- rungekutak4(fp,y0,desde,h)

par(mfrow=c(1,2))
plot(resultado[[3]], type = "l", col="royalblue")
points(resultado[[2]], col="red")
freal <- function(x) 2.1782^(x^2)/2.1782
xreal <- resultado[[3]][,1]
yreal <- sapply(xreal, freal)
plot(resultado[[2]],col="red")
lines(xreal,yreal,type = "l", col="royalblue")

par(mfrow = c(1,1))
plot(resultado[[3]], type = "l", col = "royalblue")
lines(xreal,yreal,type = "l",col = "red")
