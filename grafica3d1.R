
x <- y <- seq(0,1,length.out = 20)
f <- function(x,y){exp(-pi*x)*sin(pi*y)}
mz <- outer(x,y,f)
persp(x,y,mz,theta = 0, col = "royalblue")
