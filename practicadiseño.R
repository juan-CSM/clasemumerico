rm(list = ls())
A <- c(6,8,7,8)
B <- c(7,9,10,8)
C <- c(11,16,11,13)
D <- c(10,12,11,9)

df <- data.frame(A,B,C,D)
boxplot(df)

