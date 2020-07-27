###########################
###dpois
###########################
rm(list=ls())
library(animation)
library(tidyverse)
my.mu<-1:30
my.mu<-c(my.mu,rev(my.mu))
saveGIF({
  ani.options(interval = 0.2, nmax = length(my.mu))
  ## use a loop to create images one by one
  for (i in 1:ani.options('nmax')) {
    xrange<-0:50
    param<-my.mu[i]
    dtf_pois2<-data.frame(x=xrange,
                          y=dpois(xrange,param),
                          parameter=factor(rep(param,length(xrange))))
    p1<-dtf_pois2%>%ggplot(aes(x=x,y=y,color=parameter))+geom_point()+
      theme_bw()+ labs(color = expression(lambda),
                       y=expression(paste("f(x|",lambda,")")))+
      ylim(0,.4)
    print(p1)
    ani.pause()   ## pause for a while ('interval')
  }
}, interval = 0.05, movie.name = 'dpois.gif', ani.width = 480, ani.height = 400)

###########################
###dnorm
###########################
rm(list=ls())
library(animation)
library(tidyverse)

mu_seq<-c(seq(0,15,by = 1.5),seq(15,0,by = -1.5),seq(0,-15,by = -1.5),rev(seq(0,-15,by = -1.5)))
sig_seq<-c(rev(seq(0.15,225,length.out = 20)),seq(0.15,225,length.out = 20))
dfp<-data.frame(mu=c(mu_seq,
                     rep(0,length(sig_seq))),
                sig=c(rep(225,length(mu_seq)),sig_seq))

saveGIF({
  ani.options(interval = 0.2, nmax = nrow(dfp))
  ## use a loop to create images one by one
  for (i in 1:ani.options('nmax')) {
    m1<-dfp[i,1]
    sd1<-sqrt(dfp[i,2])
    weight_lim<-c(-40, 40)
    pdnorm1<-ggplot(data = data.frame(weight = weight_lim), aes(weight)) +
      stat_function(fun = dnorm, n = 101, args = list(mean = m1, sd = sd1),aes(color="black")) +
      ylab(expression(paste("f(x|",mu,",",sigma^2,")"))) +xlab("x") +  
      scale_color_identity(name = "",
                           breaks = c("black", "red", "blue","green"),
                           labels = c(substitute(paste(mu,"= ",m1,", ",sigma^2,"= ",sd1),list(m1=format(round(m1, 1), nsmall = 2),sd1=format(round(sd1,3), nsmall = 3)))),
                           guide = "legend")+
      ylim(0,.125)+
      theme_bw()
    print(pdnorm1)
    ani.pause()   ## pause for a while ('interval')
  }
}, interval = 0.05, movie.name = 'dnorm.gif', ani.width = 580, ani.height = 350)



