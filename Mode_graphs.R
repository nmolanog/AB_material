rm(list=ls())
options(max.print=999999)
library(pacman)
p_load(tidyverse)
p_load(kableExtra)
p_load(knitr)
p_load(latex2exp)   
p_load(ggrepel)
p_load(magick)
set.seed(150)

dtf_pois<-data.frame(x=0:11,y=dpois(0:11,3))

pdfpoissE1<-dtf_pois%>%ggplot(aes(x=x,y=y))+geom_point()+
  ylab("f(x)")+ theme_bw()+
  theme(axis.title.y = element_text(angle=0))+ ggtitle(substitute(paste("A) Poisson, ", lambda," = ",v),list(v=3)))+
  annotate("segment", x = c(3), xend = c(3), 
           y = c(0), yend = c(0.075), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = 3, y = 0.09, 
           label = paste0("Moda(X)= ",3),size=2.5)

dtf_pois<-data.frame(x=0:20,y=dpois(0:20,9))

pdfpoissE2<-dtf_pois%>%ggplot(aes(x=x,y=y))+geom_point()+
  ylab("f(x)")+ theme_bw()+
  theme(axis.title.y = element_text(angle=0))+ ggtitle(substitute(paste("B) Poisson, ", lambda," = ",v),list(v=9)))+
  annotate("segment", x = c(9), xend = c(9), 
           y = c(0), yend = c(0.075), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = 9, y = 0.09, 
           label = paste0("Moda(X)= ",9),size=2.5)

weight_lim<-c(5, 55)
mymu<-mean(weight_lim)
pdnormE<-ggplot(data = data.frame(x = weight_lim), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mymu, sd = 8))+
  ggtitle(substitute(paste("C) Normal, ", mu," = ",v,", ",sigma^2,"= ",s),list(v=mymu,s=8^2)))+
  ylab("f(x)")+
  annotate("segment", x = c(mymu), xend = c(mymu), 
           y = c(0), yend = c(0.02), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = mymu, y = 0.022, 
           label = paste0("Moda(X)= ",mymu),size=2.5)+
  theme_bw()

apar<-c(1,1.5,7.5,9)
bpar<-c(2,20,1.3,0.5)
gammaMode<-(apar-1)*bpar
gmaxlim<-c(0, 25)
pgmaE1<-ggplot(data = data.frame(x = gmaxlim), aes(x)) +
  stat_function(fun = dgamma, n = 101, args = list(shape = apar[1], scale = bpar[1]))+
  ggtitle(substitute(paste("D) Gamma, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[1],s=bpar[1])))+
  ylab("f(x)")+
  annotate("segment", x = c(gammaMode[1]), xend = c(5), 
           y = c(0), yend = c(.2), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = 5, y = .22, 
           label = paste0("Moda(X)= ",gammaMode[1]),size=2.5)+
  theme_bw()

gmaxlim2<-c(0,90)
pgmaE2<-ggplot(data = data.frame(x = gmaxlim2), aes(x)) +
  stat_function(fun = dgamma, n = 101, args = list(shape = apar[2], scale = bpar[2]))+
  ggtitle(substitute(paste("E) Gamma, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[2],s=bpar[2])))+
  ylab("f(x)")+
  annotate("segment", x = c(gammaMode[2]), xend = c(gammaMode[2]), 
           y = c(0), yend = c(0.03), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = gammaMode[2], y = 0.033, 
           label = paste0("Moda(X)= ",gammaMode[2]),size=2.5)+
  theme_bw()

pgmaE3<-ggplot(data = data.frame(x = gmaxlim), aes(x)) +
  stat_function(fun = dgamma, n = 101, args = list(shape = apar[3], scale = bpar[3]))+
  ggtitle(substitute(paste("F) Gamma, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[3],s=bpar[3])))+
  ylab("f(x)")+
  annotate("segment", x = c(gammaMode[3]), xend = c(gammaMode[3]), 
           y = c(0), yend = c(0.04), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = gammaMode[3], y = 0.05, 
           label = paste0("Moda(X)= ",gammaMode[3]),size=2.5)+
  theme_bw()

apar<-c(0.7,2.5,5)
bpar<-c(0.7,5,1)
betaMode<-(apar-1)/(apar+bpar-2)
btaxlim<-c(0, 1)
pbtaE1<-ggplot(data = data.frame(x = btaxlim), aes(x)) +
  stat_function(fun = dbeta, n = 101, args = list(shape1 = apar[1], shape2 = bpar[1]))+
  ggtitle(substitute(paste("G) Beta, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[1],s=bpar[1])))+
  ylab("f(x)")+
  annotate("segment", x = c(btaxlim), xend = c(.25,.75), 
           y = c(0,0), yend = c(1.6,1.6), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = c(.25,.75), y = c(1.7,1.7), 
           label = paste0("Moda(X)= ",btaxlim),size=2.5)+
  theme_bw()

pbtaE2<-ggplot(data = data.frame(x = btaxlim), aes(x)) +
  stat_function(fun = dbeta, n = 101, args = list(shape1 = apar[2], shape2 = bpar[2]))+
  ggtitle(substitute(paste("H) Beta, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[2],s=bpar[2])))+
  ylab("f(x)")+
  annotate("segment", x = c(betaMode[2]), xend = c(betaMode[2]), 
           y = c(0), yend = c(1.2), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = betaMode[2], y = 1.3, 
           label = paste0("Moda(X)= ",round(betaMode[2],3)),size=2.5)+
  theme_bw()

pbtaE3<-ggplot(data = data.frame(x = btaxlim), aes(x)) +
  stat_function(fun = dbeta, n = 101, args = list(shape1 = apar[3], shape2 = bpar[3]))+
  ggtitle(substitute(paste("I) Beta, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[3],s=bpar[3])))+
  ylab("f(x)")+
  annotate("segment", x = c(betaMode[3]), xend = c(.7), 
           y = c(0), yend = c(2), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = .7, y = 2.1, 
           label = paste0("Moda(X)= ",round(betaMode[3],3)),size=2.5)+
  theme_bw()

gridExtra::grid.arrange(pdfpoissE1,pdfpoissE2, pdnormE, pgmaE1, pgmaE2, pgmaE3, pbtaE1, pbtaE2, pbtaE3, 
                        ncol = 3, nrow = 3)