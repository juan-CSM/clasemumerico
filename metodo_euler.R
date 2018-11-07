rm(list = ls())
setwd("/Users/juc1987/prog/progr")
source("./sources/numerico.R")

fp <- function(t,y){
  4*(t^2 + 1) + 0*y
}

y0 <- 1
yt <- c(y0)
a <- 0
b <- 4
h <- 0.25
x <- seq(a,b,h)
for (i in 1:(length(x)-1)) {
  yt <- c(yt,yt[i]+h*fp(x[i],yt[i]))
}

y <- yt

puntos <- cbind(x,y)

poli <- pinterlagrange(puntos)
