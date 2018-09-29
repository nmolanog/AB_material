rm(list=ls())
library(animation)
library(tidyverse)
my.mu<-seq(-15,15,length.out = 20)
my.mu<-c(my.mu,rev(my.mu))
sd_dif<-sqrt(7.5^2+3.3^2)

saveGIF({
  ani.options(interval = 0.2, nmax = length(my.mu))
  ## use a loop to create images one by one
  for (i in 1:ani.options('nmax')) {
    p1<-ggplot(data = data.frame(x = c(-50,60)), aes(x)) +
      stat_function(fun = dnorm, args = list(mean = my.mu[i],sd=sd_dif),color="green")+
      stat_function(fun = dnorm, args = list(mean = 0,sd=sd_dif),color="red")+
      theme_bw()
    print(p1)
    ani.pause()   ## pause for a while ('interval')
  }
}, interval = 0.05, movie.name = 'bm_demo.gif', ani.width = 600, ani.height = 600)

