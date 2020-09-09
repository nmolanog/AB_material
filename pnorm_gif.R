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
  ylab("f(x)") +xlab("x") + 
  scale_x_continuous(breaks=seq(weight_lim[1],weight_lim[2], by=5)) + 
  stat_function(fun = dnorm, args = list(mean = mm,sd=sdm),
                xlim = c(weight_lim[1],xrange[1]),
                geom = "area",fill="red",alpha=0.5)+
  annotate("text", x = 40, y = .02, 
           label = deparse(lab), parse=TRUE,
           size=4)+
  theme_bw()
p1

dfpnorm<-data.frame(x=seq(30,110,length.out = 1000),y=pnorm(seq(30,110,length.out = 1000),mm,sdm))
dfpnorm$z<-(dfpnorm$x<xrange[1]) %>% as.numeric() %>% factor

dfpnorm%>%ggplot(aes(x=x,y=y,color=z))+
  geom_line()+ 
  theme_bw()+
  ylab("F(x)")+
  theme(legend.position = "none")+
  scale_color_manual(values=c("white", "black"))

mf<-55
mm<-70
weight_lim<-c(30, 125)
xrange<-c(seq(30,125,by=2),rep(125,10))
xrange<-c(xrange,rev(xrange))
dfpnorm<-data.frame(x=seq(30,125,length.out = 1000),y=pnorm(seq(30,125,length.out = 1000),mm,sdm))
saveGIF({
  ani.options(interval = 0.2, nmax = length(xrange))
  ## use a loop to create images one by one
  for (i in 1:ani.options('nmax')) {
    vv <- format(xrange[i], nsmall = 1)
    ss <- round(pnorm(xrange[i],mm,sdm),4)
    lab <- bquote(P(X <= .(vv)) == .(ss)) 
    
    p1<-ggplot(data = data.frame(weight = weight_lim), aes(weight)) +
      stat_function(fun = dnorm, n = 101, args = list(mean = mm, sd = sdm),color=1) +
      ylab("f(x)") +xlab("x") + 
      ylim(0,.038)+
      scale_x_continuous(breaks=seq(weight_lim[1],weight_lim[2], by=10)) + 
      stat_function(fun = dnorm, args = list(mean = mm,sd=sdm),
                    xlim = c(weight_lim[1],xrange[i]),
                    geom = "area",fill="red",alpha=0.5)+
      annotate("text", x = 70, y = .036, 
               label = deparse(lab), parse=TRUE,
               size=4)+
       ggtitle("Función de densidad")+
      theme_bw()
    
    dfpnorm$z<-(dfpnorm$x<xrange[i]) %>% as.numeric() %>% factor
    p2<-dfpnorm%>%ggplot(aes(x=x,y=y,color=z))+
      geom_line()+ 
      theme_bw()+
      ylab("F(x)")+
      theme(legend.position = "none")+
      scale_color_manual(values=c("grey", "black"))+
      ggtitle("Función de distribución")
    
    gridExtra::grid.arrange(grobs =list(p1,p2),nrow=1,ncol=2)
    ani.pause()   ## pause for a while ('interval')
  }
}, interval = 0.05, movie.name = 'pnorm.gif', ani.width = 600, ani.height = 300)


gridExtra::grid.arrange(grobs =list(p1),nrow=1,ncol=2)
