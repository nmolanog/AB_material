rm(list=ls())
library(animation)
library(tidyverse)

nsize<-c(seq(30,100,by=10),seq(100,1000,by=100),2000,4000,8000,10000)
weight_lim<-c(30, 120)


rngdf<-data.frame(x=rnorm(50,70,10))
ggplot(data = data.frame(weight = weight_lim), aes(weight)) +
  ylab("f(x)") +xlab("x") + geom_histogram(data = rngdf,aes(x=x,y=..density..),color="black",fill="white",bins=nclass.scott(rngdf$x))+
  stat_function(fun = dnorm, n = 101, args = list(mean = 70, sd = 10),color="blue") +
  theme_bw()

nsize<-c(seq(30,100,by=10),seq(100,1000,by=100),2000,4000,8000,10000,20000,50000,90000)
nsize<-c(nsize,rev(nsize))
weight_lim<-c(30, 120)
saveGIF({
  ani.options(interval = 0.5, nmax = length(nsize))
  ## use a loop to create images one by one
  for (i in 1:ani.options('nmax')) {
    rngdf<-data.frame(x=rnorm(nsize[i],70,10))
    pdnorm1<-ggplot(data = data.frame(weight = weight_lim), aes(weight)) +
      ylab("f(x)") +xlab("x") + geom_histogram(data = rngdf,aes(x=x,y=..density..),color="black",fill="white",bins=nclass.scott(rngdf$x))+
      stat_function(fun = dnorm, n = 101, args = list(mean = 70, sd = 10),color="blue") +
      ylim(0,.05)+xlim(25,125)+
      annotate("text", x = c(110), y = c(.035), 
               label =paste0("n= ",nsize[i]), size=6)+
      theme_bw()
      
    print(pdnorm1)
    ani.pause()   ## pause for a while ('interval')
  }
}, interval = 0.05, movie.name = 'dnorm_hist.gif', ani.width = 580, ani.height = 350)
