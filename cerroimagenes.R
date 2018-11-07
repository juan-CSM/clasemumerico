library(ggplot2)
library(jpeg)
rm(list = ls())
setwd("/Users/juc1987/prog/progr")
source("./sources/numerico.R")

dibujo <- readJPEG("./archivos/Cerro.jpg")


dibujo[dibujo[,,3]>.6] <- 1
x <- seq(1,ncol(dibujo[,,3]), 40)
altura <- nrow(dibujo[,,3])
y <- c()
for (i in x) {
  y <- c(y,(altura-min(which(dibujo[,i,3] !=1))))
}

plot(x,y)

graficacerro <- graficaspli(x,y,10,"Cerro de la silla")

writeJPEG(dibujo,target = "./archivos/Cerro2.jpg")
