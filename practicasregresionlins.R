rm(list = ls())
library(MASS)
library(ggplot2)
data("Cars93")
ggplot(data = Cars93, aes(x = Weight, y=Horsepower)) + geom_point(colour="red") + ggtitle("Diagrama de dispersion")+
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))
par(mfrow=c(1,2))
hist(Cars93$Weight,breaks = 10 ,main="",xlab = "weight",border = "red")
hist(Cars93$Horsepower, breaks = 10,main="",xlab = "horsepower", border = "blue")
qqnorm(Cars93$Weight, main = "Weight", col = "darkred")
qqline(Cars93$Weight)
qqnorm(Cars93$Horsepower, main = "Horsepower", col = "blue")
qqline(Cars93$Horsepower)

par(mfrow = c(1,1))


ggplot(data = Cars93, aes(x = Weight, y = Horsepower)) + 
  geom_point(colour = "red4") +
  geom_segment(aes(x = 2000, y = 70, xend = 3100, yend = 300),linetype="dashed",colour="blue") +
  geom_segment(aes(x = 2000, y = 45, xend = 4100, yend = 100),linetype="dashed") +
  ggtitle("Diagrama de dispersión") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


rm(list = ls())
data("iris")
pairs(iris)
