rm(list=ls())
library(animation)
library(tidyverse)

nsize<-c(1,seq(5,100,by=5),200,500,1000)
nsize<-c(nsize,rev(nsize))
weight_lim<-c(11, 36)

ggplot(data = data.frame(weight = weight_lim), aes(weight)) +
  ylab("f(imc)") +xlab("imc") + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 23, sd = 3),aes(color="blue")) +
  stat_function(fun = dnorm, n = 1000, args = list(mean = 23, sd = 3/sqrt(1000)),aes(color="red")) +
  scale_color_identity(name = "",
                       breaks = c("blue", "red"),
                       labels = c("IMC",expression(bar(IMC))),
                       guide = "legend")+
  ylim(0,1)+
  scale_x_continuous(breaks=seq(11,36,by=2))+
  geom_segment(aes(x=23,xend=23,y=0,yend=.025),linetype=1)+
  geom_segment(aes(x=23,xend=23,y=0,yend=.025),linetype=1)+
  annotate("text", x = c(23), y = c(.04), 
           label ="E(IMC)", size=3)+
  annotate("text", x = c(31), y = c(.75), 
           label =paste0("n= ",50), size=6)+
  theme_bw()


saveGIF({
  ani.options(interval = 0.5, nmax = length(nsize))
  ## use a loop to create images one by one
  for (i in 1:ani.options('nmax')) {
    
    pdnorm1<-ggplot(data = data.frame(weight = weight_lim), aes(weight)) +
      ylab("f(imc)") +xlab("imc") + 
      stat_function(fun = dnorm, n = 101, args = list(mean = 23, sd = 3),aes(color="blue")) +
      stat_function(fun = dnorm, n = 9000, args = list(mean = 23, sd = 3/sqrt(nsize[i])),aes(color="red")) +
      scale_color_identity(name = "",
                           breaks = c("blue", "red"),
                           labels = c("IMC",expression(bar(IMC))),
                           guide = "legend")+
      ylim(0,1)+
      scale_x_continuous(breaks=seq(11,36,by=2))+
      geom_segment(aes(x=23,xend=23,y=0,yend=.025),linetype=1)+
      geom_segment(aes(x=23,xend=23,y=0,yend=.025),linetype=1)+
      annotate("text", x = c(23), y = c(.04), 
               label ="E(IMC)", size=3)+
      annotate("text", x = c(31), y = c(.75), 
               label =paste0("n= ",nsize[i]), size=6)+
      theme_bw()
    
    print(pdnorm1)
    ani.pause()   ## pause for a while ('interval')
  }
}, interval = 0.05, movie.name = 'dnorm_prom.gif', ani.width = 500, ani.height = 300)
