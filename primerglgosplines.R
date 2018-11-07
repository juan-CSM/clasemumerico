rm(list = ls())
setwd("/Users/juc1987/prog/progr")
source("./sources/numerico.R")

splinescubicos <- function(x,y){
  n <- length(x)
  h <- rep(0,n-1)
  b <- h
  for (i in 1:n-1) {
    h[i] <- x[i+1] - x[i]
    b[i] <- 6*(y[i+1] - y[i])/h[i]
  }
  
  v <- u <- rep(0,n-2)
  u[1] <- 2*(h[1] + h[2])
  v[1] <- b[1] - b[2]
  for (i in 2:length(h)) {
    u[i] <- 2*(h[i] + h[i-1]) - ((h[i-1])^2)/u[i-1]
    v[i] <- b[i] - b[i-1] - h[i-1]*v[i-1]/u[i-1]
  }
  
  z <- rep(0,n)
  for (i in seq(n-1,2,-1)) {
    z[i] <- (v[i] - h[i]*z[i + 1])/u[i]
  }
  C <- B <- A <- rep(0,n-1)
  S <- matrix(c(0),ncol = 4)
  for (i in 1:n-1) {
    A[i] <- (z[i+1]-z[i])/(6*h[i])
    B[i] <- z[i]/2
    C[i] <- -h[i]*z[i+1]/6 - h[i]*z[i]/3 + (y[i+1] - y[i])/h[i]
    temp1 <- sumpoli(c(B[i]),c(A[i])*c(1,-x[i]))
    temp2 <- multiplicapoli(c(1,-x[i]),temp1)
    temp3 <- sumpoli(c(C[i]),temp2)
    temp4 <- multiplicapoli(c(1,-x[i]),temp3)
    temporal <- sumpoli(y[i],temp4)
    if(length(temporal)<4){
      temporal <- llenapoli(temporal,3)
    }
    S <- rbind(S,temporal)
  }
  resultado <- S[3:nrow(S),]
  rownames(resultado) <- 1:nrow(resultado)
  resultado
}

graficaspli <- function(x,y,pintermedios){
  matspli <- splinescubicos(x,y)
  ys <- xs <- c()
  for (i in 1:nrow(matspli)) {
    xt <- seq(x[i],x[i+1],length.out = pintermedios)
    ft <- function(x) evalpoli(x,matspli[i,])
    yt <- sapply(xt, ft)
    xs <- c(xs,xt)
    ys <- c(ys,yt)
  }
  print(length(xs))
  print(length(ys))
  plot.new()
  plot.window(xlim = range(x),ylim = range(y))
  plot(xs,ys)
  points(x,y,col="red")
}

t <- seq(-10,10,length.out = 50)

f <- function(x) (cos(x)+x^2-x^3-4)
tempx <- runif(10,-10,10)
tempx <- tempx[order(tempx)]
tempy <- sapply(tempx,f)
y <- sapply(t, f)
salida <- splinescubicos(t,y)
puntosgraf <- graficaspli(t,y,10)

puntosgraf2 <- graficaspli(tempx,tempy,10)





