library(pacman)
p_load(tidyverse)
p_load(kableExtra)
p_load(knitr)
p_load(latex2exp)   
p_load(ggrepel)
p_load(magick)
set.seed(150)

dtf_pois<-data.frame(x=0:11,y=dpois(0:11,3))

pdfpoissMed1<-dtf_pois%>%ggplot(aes(x=x,y=y))+geom_point()+
  ylab("f(x)")+ theme_bw()+
  theme(axis.title.y = element_text(angle=0))+ ggtitle(substitute(paste("Poisson, ", lambda," = ",v),list(v=3)))+
  annotate("segment", x = c(0:3), xend = c(0:3), 
           y = c(rep(0,4)), yend = c(dpois(0:3,3)), colour = "red", size=4, alpha=0.6)+
  annotate("text", x = 6, y = 0.15, 
           label = paste0("med(X)= ",3),size=5)+theme_bw()


dtf_pois<-data.frame(x=0:20,y=dpois(0:20,9))

pdfpoissMed2<-dtf_pois%>%ggplot(aes(x=x,y=y))+geom_point()+
  ylab("f(x)")+ theme_bw()+
  theme(axis.title.y = element_text(angle=0))+ ggtitle(substitute(paste("B) Poisson, ", lambda," = ",v),list(v=9)))+
  annotate("segment", x = c(0:9), xend = c(0:9), 
           y = c(rep(0,10)), yend = c(dpois(0:9,9)), colour = "red", size=4, alpha=0.6)+
  annotate("text", x = 14, y = 0.1, 
           label = paste0("med(X)= ",9),size=5)+theme_bw()

weight_lim<-c(5, 55)
mymu<-mean(weight_lim)
pdnormMed<-ggplot(data = data.frame(x = weight_lim), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mymu, sd = 8))+
  ggtitle(substitute(paste("C) Normal, ", mu," = ",v,", ",sigma^2,"= ",s),list(v=mymu,s=8^2)))+
  ylab("f(x)")+
  stat_function(fun = dnorm, args = list(mean = mymu,sd=8),
                xlim = c(weight_lim[1],qnorm(.5,mymu,8)),
                geom = "area",fill="red",alpha=0.5)+
  annotate("text", x = 45, y = 0.04, 
           label = paste0("med(X)= ",qnorm(.5,mymu,8)),size=5)+
  theme_bw()

apar<-c(1,1.5,7.5,9)
bpar<-c(2,20,1.3,0.5)

gmaxlim<-c(0, 25)
pgmaMed1<-ggplot(data = data.frame(x = gmaxlim), aes(x)) +
  stat_function(fun = dgamma, n = 101, args = list(shape = apar[1], scale = bpar[1]))+
  ggtitle(substitute(paste("D) Gamma, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[1],s=bpar[1])))+
  ylab("f(x)")+
  stat_function(fun = dgamma, args = list(shape = apar[1], scale = bpar[1]),
                xlim = c(gmaxlim[1],qgamma(.5,shape = apar[1], scale = bpar[1])),
                geom = "area",fill="red",alpha=0.5)+
  annotate("text", x = 8, y = 0.4, 
           label = paste0("med(X)= ",qgamma(.5,shape = apar[1], scale = bpar[1])%>%round(2)),size=5)+
  theme_bw()

gmaxlim2<-c(0,90)
pgmaMed2<-ggplot(data = data.frame(x = gmaxlim2), aes(x)) +
  stat_function(fun = dgamma, n = 101, args = list(shape = apar[2], scale = bpar[2]))+
  ggtitle(substitute(paste("E) Gamma, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[2],s=bpar[2])))+
  ylab("f(x)")+
  stat_function(fun = dgamma, args = list(shape = apar[2], scale = bpar[2]),
                xlim = c(gmaxlim2[1],qgamma(.5,shape = apar[2], scale = bpar[2])),
                geom = "area",fill="red",alpha=0.5)+
  annotate("text", x = 38, y = 0.02, 
           label = paste0("med(X)= ",qgamma(.5,shape = apar[2], scale = bpar[2])%>%round(2)),size=5)+
  theme_bw()
  
pgmaMed3<-ggplot(data = data.frame(x = gmaxlim), aes(x)) +
  stat_function(fun = dgamma, n = 101, args = list(shape = apar[3], scale = bpar[3]))+
  ggtitle(substitute(paste("F) Gamma, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[3],s=bpar[3])))+
  ylab("f(x)")+
  stat_function(fun = dgamma, args = list(shape = apar[3], scale = bpar[3]),
                xlim = c(gmaxlim[1],qgamma(.5,shape = apar[3], scale = bpar[3])),
                geom = "area",fill="red",alpha=0.5)+
  annotate("text", x = 18, y = 0.075, 
           label = paste0("med(X)= ",qgamma(.5,shape = apar[3], scale = bpar[3])%>%round(2)),size=5)+
  theme_bw()

apar<-c(0.7,2.5,5)
bpar<-c(0.7,5,1)
btaxlim<-c(0, 1)
pbtaMed1<-ggplot(data = data.frame(x = btaxlim), aes(x)) +
  stat_function(fun = dbeta, n = 101, args = list(shape1 = apar[1], shape2 = bpar[1]))+
  ggtitle(substitute(paste("G) Beta, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[1],s=bpar[1])))+
  ylab("f(x)")+
  stat_function(fun = dbeta, args = list(shape1 = apar[1], shape2 = bpar[1]),
                xlim = c(gmaxlim[1],qbeta(.5,shape1 = apar[1], shape2 = bpar[1])),
                geom = "area",fill="red",alpha=0.5)+
  annotate("text", x = .5, y = 2, 
           label = paste0("med(X)= ",qbeta(.5,shape1 = apar[1], shape2 = bpar[1])%>%round(2)),size=5)+
  theme_bw()

pbtaMed2<-ggplot(data = data.frame(x = btaxlim), aes(x)) +
  stat_function(fun = dbeta, n = 101, args = list(shape1 = apar[2], shape2 = bpar[2]))+
  ggtitle(substitute(paste("H) Beta, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[2],s=bpar[2])))+
  ylab("f(x)")+
  stat_function(fun = dbeta, args = list(shape1 = apar[2], shape2 = bpar[2]),
                xlim = c(gmaxlim[1],qbeta(.5,shape1 = apar[2], shape2 = bpar[2])),
                geom = "area",fill="red",alpha=0.5)+
  annotate("text", x = .7, y = 2, 
           label = paste0("med(X)= ",qbeta(.5,shape1 = apar[2], shape2 = bpar[2])%>%round(2)),size=5)+
  theme_bw()


pbtaMed3<-ggplot(data = data.frame(x = btaxlim), aes(x)) +
  stat_function(fun = dbeta, n = 101, args = list(shape1 = apar[3], shape2 = bpar[3]))+
  ggtitle(substitute(paste("I) Beta, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[3],s=bpar[3])))+
  ylab("f(x)")+
  stat_function(fun = dbeta, args = list(shape1 = apar[3], shape2 = bpar[3]),
                xlim = c(gmaxlim[1],qbeta(.5,shape1 = apar[3], shape2 = bpar[3])),
                geom = "area",fill="red",alpha=0.5)+
  annotate("text", x = .5, y = 4, 
           label = paste0("med(X)= ",qbeta(.5,shape1 = apar[3], shape2 = bpar[3])%>%round(2)),size=5)+
  theme_bw()

gridExtra::grid.arrange(pdfpoissMed1,pdfpoissMed2, pdnormMed, pgmaMed1, pgmaMed2, pgmaMed3, pbtaMed1, pbtaMed2, pbtaMed3, 
                        ncol = 3, nrow = 3)