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

rm(list=ls())
library(animation)
library(tidyverse)
library(latex2exp)
my.mu<-seq(0,-3,length.out = 20)
my.mu<-c(my.mu,rev(my.mu))
sd_dif<-4/sqrt(50)

p1<-ggplot(data = data.frame(x = c(-5, 2.7)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 0,sd=sd_dif),color="red")+ylab(TeX("$f(\\bar{Dif})$"))+xlab(TeX("$\\bar{Dif}$"))+
theme_bw()

saveGIF({
  ani.options(interval = 1.2, nmax = length(my.mu))
  ## use a loop to create images one by one
  for (i in 1:ani.options('nmax')) {
    p1+stat_function(fun = dnorm, args = list(mean = my.mu[i],sd=sd_dif),color="blue")+
      geom_segment(aes(x=qnorm(.05,0,sd_dif),xend=qnorm(.05,0,sd_dif),y=0,yend=max(dnorm(qnorm(.05,0,sd_dif),0,sd_dif),dnorm(qnorm(.05,0,sd_dif),my.mu[i],sd_dif))))+
      annotate("text", x = 2, y =.6, 
               label =  TeX('$H_0: E(Dif) = 0$'), size=4 ,color="red")+
      annotate("text", x = 2, y =.55, 
               label = TeX(sprintf('$H_a: E(Dif) =%.3f$', my.mu[i])), size=4, color="blue")+
      annotate("text", x = 2, y =.5, 
               label = TeX(sprintf('$\\beta = %.3f$', pnorm(qnorm(.05,0,sd_dif),my.mu[i],sd_dif,lower.tail = F))) , size=4)+
      annotate("text", x = 2, y =.45, 
               label = TeX(sprintf('$1-\\beta = %.3f$', pnorm(qnorm(.05,0,sd_dif),my.mu[i],sd_dif,lower.tail = T))) , size=4)+
    stat_function(fun = dnorm, args = list(mean = my.mu[i],sd=sd_dif),
                xlim = c(qnorm(.05,0,sd_dif),2),
                geom = "area",fill="orange",alpha=0.5)->p2
    print(p2)
    ani.pause()   ## pause for a while ('interval')
  }
}, interval = 1.2, movie.name = 'power.gif', ani.width = 600, ani.height = 400)


rm(list=ls())
library(animation)
library(tidyverse)
library(latex2exp)
n<-c(10,12,15,18,20,23,25,28,30,35,40,45,50,60,70,80,90,100,110,120,130,140)
n<-c(n,rev(n))
sd_dif<-40

saveGIF({
  ani.options(interval = 1.5, nmax = length(n))
  ## use a loop to create images one by one
  for (i in 1:ani.options('nmax')) {
p1<-ggplot(data = data.frame(x = c(-6, 4)), aes(x)) +xlim(-6, 4)+
  stat_function(fun = dnorm, args = list(mean = 0,sd=sd_dif/n[i]),color="red")+ylab(TeX("$f(\\bar{Dif})$"))+xlab(TeX("$\\bar{Dif}$"))+
  stat_function(fun = dnorm, args = list(mean = -1.7,sd=sd_dif/n[i]),color="blue")+
  geom_segment(aes(x=qnorm(.05,0,sd_dif/n[i]),xend=qnorm(.05,0,sd_dif/n[i]),y=0,yend=max(dnorm(qnorm(.05,0,sd_dif/n[i]),0,sd_dif/n[i]),dnorm(qnorm(.05,0,sd_dif/n[i]),-1.7,sd_dif/n[i]))))+
  annotate("text", x = 3, y =dnorm(0,0,sd_dif/n[i]), 
           label =  TeX('$H_a: E(Dif) = -1.7$'), size=4 ,color="blue")+
  annotate("text", x = 3, y =dnorm(0,0,sd_dif/n[i])*9/10, 
           label =  TeX('$H_0: E(Dif) = 0$'), size=4 ,color="red")+
  annotate("text", x = 3, y =dnorm(0,0,sd_dif/n[i])*8/10, 
           label = TeX(sprintf('$1-\\beta = %.3f$', pnorm(qnorm(.05,0,sd_dif/n[i]),-1.7,sd_dif/n[i],lower.tail = T))) , size=4)+
  annotate("text", x = 3, y =dnorm(0,0,sd_dif/n[i])*7/10, 
           label = TeX(sprintf('$\\beta = %.3f$', pnorm(qnorm(.05,0,sd_dif/n[i]),-1.7,sd_dif/n[i],lower.tail = F))) , size=4)+
  annotate("text", x = 3, y =dnorm(0,0,sd_dif/n[i])*6/10, 
           label = TeX(sprintf('$n = %d$', n[i])) , size=4)+
  stat_function(fun = dnorm, args = list(mean = -1.7,sd=sd_dif/n[i]),
                xlim = c(qnorm(.05,0,sd_dif/n[i]),5),
                geom = "area",fill="orange",alpha=0.5)+
  theme_bw()

print(p1)
ani.pause()   
  }
}, interval = 1.5, movie.name = 'power2.gif', ani.width = 600, ani.height = 400)
