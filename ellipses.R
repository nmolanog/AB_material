rm(list=ls())
library(tidyverse)
library(ellipse)

plot(ellipse(0.8), type = 'l')
points(ellipse(0.8,level=.8),col=2, type = 'l')
points(ellipse(0.8,level=.4),col=3, type = 'l')
points(ellipse(0.8,level=.2),col=4, type = 'l')
points(ellipse(0.8,level=.05),col=5, type = 'l')
points(ellipse(0.8,level=.01),col=6, type = 'l')

dat1<-data.frame(ellipse(0.8))
colnames(dat1)<-c("x","y")
dat1%>%ggplot(aes(x,y))+geom_point()

dat1[order(dat1$x,dat1$y),]%>%ggplot(aes(x,y))+geom_line()

library(mvtnorm)

x1<-seq(-3,3,length.out = 50)
y1<-x1
f1<-function(x,y){dmvnorm(c(x,y),sigma =zigma)}
zigma<-matrix(c(1,.8,.8,1),ncol = 2)
z1<-outer(x1,y1,Vectorize(f1))


par(mfrow=c(1,2))
persp(x1, y1, z1, theta = 0, phi = 50, expand = 0.5, col = "lightblue",xlab = "x", ylab = "y", zlab = "z",r=6)
plot(ellipse(0.8,level = .9), type = 'l',asp = 1,xlim = c(-3,3),ylim= c(-3,3))
for(i in seq(0.01,.9,length.out = 10)){
  points(ellipse(0.8,level=i), type = 'l')
}
