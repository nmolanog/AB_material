rm(list=ls())
library(animation)
library(tidyverse)

mm<-70
sdm<-12
weight_lim<-c(30, 110)
xrange<-55

vv <- format(xrange, nsmall = 1)
ss <- round(pnorm(xrange,mm,sdm),4)
lab <- bquote(P(X <= .(vv)) == .(ss)) 

p1<-ggplot(data = data.frame(weight = weight_lim), aes(weight)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mm, sd = sdm),color=1) +
  ylab("f(weight)") + scale_x_continuous(breaks=seq(weight_lim[1],weight_lim[2], by=5)) + 
  stat_function(fun = dnorm, args = list(mean = mm,sd=sdm),
                xlim = c(weight_lim[1],xrange[1]),
                geom = "area",fill="red",alpha=0.5)+
  annotate("text", x = 40, y = .02, 
           label = deparse(lab), parse=TRUE,
            size=4)+
  theme_bw()
p1


  mf<-55
  mm<-70
  weight_lim<-c(30, 110)
  xrange<-c(seq(30,mm,by=.5),rep(mm,10))
  saveGIF({
    ani.options(interval = 0.2, nmax = length(xrange))
    ## use a loop to create images one by one
    for (i in 1:ani.options('nmax')) {
      vv <- format(xrange[i], nsmall = 1)
      ss <- round(pnorm(xrange[i],mm,sdm),4)
      lab <- bquote(P(X <= .(vv)) == .(ss)) 
      
      p1<-ggplot(data = data.frame(weight = weight_lim), aes(weight)) +
        stat_function(fun = dnorm, n = 101, args = list(mean = mm, sd = sdm),color=1) +
        ylab("f(weight)") + scale_x_continuous(breaks=seq(weight_lim[1],weight_lim[2], by=5)) + 
        stat_function(fun = dnorm, args = list(mean = mm,sd=sdm),
                      xlim = c(weight_lim[1],xrange[i]),
                      geom = "area",fill="red",alpha=0.5)+
        annotate("text", x = 40, y = .02, 
                 label = deparse(lab), parse=TRUE,
                 size=4)+
        theme_bw()
      print(p1)
      ani.pause()   ## pause for a while ('interval')
    }
  }, interval = 0.05, movie.name = 'med_dnorm.gif', ani.width = 480, ani.height = 400)
