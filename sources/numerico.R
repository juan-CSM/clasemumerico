require(ggplot2)
########################################################
###
###             funciones
###
########################################################

llenapoli <- function(polinomio,grado){
  if(1+grado-length(polinomio)>0){
    resultado <- c(rep(0,1+grado-length(polinomio)),polinomio)
  }else{
    resultado <- polinomio
  }
  resultado
}

sumpoli <- function(polinomio1,polinomio2){
  grado1 <- length(polinomio1)
  grado2 <- length(polinomio2)
  if(grado1>grado2){
    p1 <- polinomio1
    p2 <- c(rep(0,grado1-grado2),polinomio2)
  }else if(grado2>grado1){
    p1 <- polinomio2
    p2 <- c(rep(0,grado2-grado1),polinomio1)
  }else{
    p1 <- polinomio1
    p2 <- polinomio2
  }
  p1+p2
}

multiplicapoli <- function(pol1,pol2){
  polsalida <- rep(0,(length(pol1)+length(pol2)-1))
  for(i in 0:(length(pol1)-1)){
    for(j in 0:(length(pol2)-1)){
      polsalida[i+j+1] <- polsalida[i+j+1] + pol1[i+1]*pol2[j+1]
    }
  }
  polsalida
}

elevpotpoli <- function(polinomio,potencia){
  resultado <- c(1)
  for (i in 1:potencia) {
    resultado <- multiplicapoli(polinomio,resultado)
  }
  resultado
}

formarpoli <- function(puntos){
  matpuntos <- matrix(c(0,0),ncol = 2,nrow = nrow(puntos))
  for (i in 1:nrow(puntos)) {
    matpuntos[i,1] <- 1
    matpuntos[i,2] <- -(puntos[i,1])
  }
  matpuntos
}

lagrange <- function(puntos){
  matres <- matrix(0,ncol = nrow(puntos))
  matpol <- formarpoli(puntos)
  for(i in 1:nrow(puntos)){
    temp <- 1
    divisor <- 1
    for (j in 1:nrow(puntos)) {
      if(i != j){
        temp <- multiplicapoli(temp,matpol[j,])
        divisor <- divisor * (puntos[i,1]-puntos[j,1])
      }
    }
    matres <- rbind(matres,temp*(puntos[i,2]/divisor))
  }
  matres[2:nrow(matres),]
}

pinterlagrange <- function(matlag){
  polinomiointer <- rep(0, ncol(matlag))
  for (i in 1:ncol(matlag)) {
    temp <- sum(matlag[,i])
    polinomiointer[i] <- temp
  }
  polinomiointer
}

polilag <- function(puntos){
  matlagrange <- lagrange(puntos)
  pinterlagrange(matlagrange)
}

evalpoli <- function(valor,polinomio){
  n <- length(polinomio)
  temporal <- rep(0,n)
  for (i in 1:n) {
    temporal[i] <- polinomio[i]*valor^(n-i)
  }
  sum(temporal)
}

pendiente <- function(pts){
  matriztemporal <- matrix(c(0),ncol = nrow(pts)-1,nrow = nrow(pts))
  matriztemporal <- cbind(pts,matriztemporal)
  k <- 1
  for (i in (nrow(pts)-1):1) {
    for (j in 1:i) {
      tempo <- (matriztemporal[j+1,k+1]-matriztemporal[j,k+1])/(matriztemporal[j+k,1]-matriztemporal[j,1])
      matriztemporal[j,k+2] <- tempo
    }
    k <- k+1
  }
  matriztemporal
}

newton <- function(pts){
  matpendientes <- pendiente(pts)
  ptsformapoli <- formarpoli(pts)
  matpolnw <- matrix(c(0),ncol = nrow(pts))
  for (i in 1:(nrow(pts)-1)) {
    temp <- c(1)
    for (j in 1:i) {
      temp <- multiplicapoli(temp,ptsformapoli[j,])
    }
    if(length(temp) < nrow(pts)){
      temp <- c(rep(0,nrow(pts)-length(temp)),temp)
    }
    matpolnw <- rbind(matpolnw,temp)
    
  }
  coeeficientes <- matpendientes[1,-c(1)]
  matpolnw[1,ncol(matpolnw)] <- 1
  for (i in 1:length(coeeficientes)) {
    matpolnw[i,] <- matpolnw[i,]*coeeficientes[i]
  }
  salida <- rep(0,ncol(matpolnw))
  for (i in 1:ncol(matpolnw)) {
    salida[i] <- sum(matpolnw[,i])
  }
  salida
}



##############   prueba

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

graficaspli <- function(x,y,pintermedios,titulo="Grafica splines cubicos"){
  matspli <- splinescubicos(x,y)
  ys <- xs <- c()
  for (i in 1:nrow(matspli)) {
    xt <- seq(x[i],x[i+1],length.out = pintermedios)
    ft <- function(x) evalpoli(x,matspli[i,])
    yt <- sapply(xt, ft)
    xs <- c(xs,xt)
    ys <- c(ys,yt)
  }
  # df <- data.frame(xs,ys)
  # p <- ggplot(data = df,aes(x=xs,y=ys)) +geom_line(colour="royalblue")
  # for (i in 1:nrow(matspli)) {
  #   p <- p + geom_segment(aes(x=x[i],y=y[i],xend=x[i+1],yend=y[i+1]),linetype="dashed",colour="red")
  #   print(i)
  # }
  # p
  numeropuntos <- paste("Generado con:",length(x),"puntos")
  par.original <- par(no.readonly = TRUE)
  par(bg="lightgrey")
  plot.new()
  plot.window(xlim = range(x),ylim = range(y), asp=1)
  plot(xs,ys, main = titulo,sub=numeropuntos, xlab = "x[i]", ylab = "y[i]", col="royalblue",type = "l",lty=1,
       frame.plot = FALSE, asp=1)
  points(x,y,col="red",type = "p",pch=16)
  par(par.original)
  cbind(xs,ys)
}


###             Ecuaciones Diferenciales
###
########################################################


rungekutak4 <- function(derivada,yinicial,rango,h){
  x <- seq(rango[1],rango[2],h)
  y <- rep(0,length(x))
  y[1] <- yinicial
  for (i in 2:length(y)) {
    k1 <- h*derivada(x[i-1],y[i-1])
    k2 <- h*derivada(x[i-1] + h/2, y[i-1] + k1/2)
    k3 <- h*derivada(x[i-1] + h/2, y[i-1] + k2/2)
    k4 <- h*derivada(x[i-1] + h, y[i] + k3)
    y[i] <- y[i-1] + (k1+2*k2+2*k3+k4)/6
  }
  puntos <- cbind(x,y)
  
  sol <- newton(puntos)
  xgra <- seq(rango[1],rango[2],h/10)
  f <- function(x) evalpoli(x,sol)
  ygra <- sapply(xgra, f)
  list(sol,puntos,cbind(xgra,ygra))
}