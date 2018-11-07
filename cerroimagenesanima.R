library(ggplot2)
library(jpeg)
rm(list = ls())
setwd("/Users/juc1987/prog/progr")
source("./sources/numerico.R")

dibujo <- readJPEG("./archivos/Cerro.jpg")
dibujo[dibujo[,,3]>.6] <- 1

repeticiones <- c(seq(100,10,-10),seq(9,1,-1))
posicion <- 1:length(repeticiones)
ceros <- c("0","00")
ajuste <- ""
for (j in posicion) {
  x <- seq(1,ncol(dibujo[,,3]), repeticiones[j])
  altura <- nrow(dibujo[,,3])
  y <- c()
  for (i in x) {
    y <- c(y,(altura-min(which(dibujo[,i,3] !=1))))
  }
  graficacerro <- graficaspli(x,y,10,"Cerro de la silla")
  
  if(j>=10 && j<100){
    ajuste <- ceros[1]
  }
  if(j<10){
    ajuste <-  ceros[2]
  }
  nombre <- paste("./archivos/gcerro/imagen",ajuste,j,".png",sep = "")
  png(nombre,height = 750, width = 1700)
  plot(graficacerro, main = "Cerro de la silla",sub = paste("grafica generada con ",length(x)," puntos",sep = ""),
       ylab = "y", xlab = "x", asp = 1, col="royalblue",type = "l", lty=1, frame.plot = FALSE)
  points(x,y, col="red",type = "p", pch=16 )
  dev.off()
}


writeJPEG(dibujo,target = "./archivos/Cerro2.jpg")
