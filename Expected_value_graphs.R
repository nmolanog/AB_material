library(pacman)
p_load(tidyverse)
p_load(kableExtra)
p_load(knitr)
p_load(latex2exp)   
p_load(ggrepel)
p_load(magick)
set.seed(150)

dtf_pois<-data.frame(x=0:11,y=dpois(0:11,3))

pdfpoissE<-dtf_pois%>%ggplot(aes(x=x,y=y))+geom_point()+
  ylab("f(x)")+ theme_bw()+
  theme(axis.title.y = element_text(angle=0))+ ggtitle(substitute(paste("Poisson, ", lambda," = ",v),list(v=3)))+
  annotate("segment", x = c(3), xend = c(3), 
           y = c(0), yend = c(0.075), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = 3, y = 0.09, 
           label = paste0("E(X)= ",3),size=5)


weight_lim<-c(5, 55)
mymu<-mean(weight_lim)
pdnormE<-ggplot(data = data.frame(x = weight_lim), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mymu, sd = 8))+
  ggtitle(substitute(paste("Normal, ", mu," = ",v,", ",sigma^2,"= ",s),list(v=mymu,s=8^2)))+
  ylab("f(x)")+
  annotate("segment", x = c(mymu), xend = c(mymu), 
           y = c(0), yend = c(0.02), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = mymu, y = 0.022, 
           label = paste0("E(X)= ",mymu),size=5)+
  theme_bw()

apar<-c(1,1.5,7.5,9)
bpar<-c(2,20,1.3,0.5)
gammaE<-apar*bpar
gmaxlim<-c(0, 25)
pgmaE1<-ggplot(data = data.frame(x = gmaxlim), aes(gmaxlim)) +
  stat_function(fun = dgamma, n = 101, args = list(shape = apar[1], scale = bpar[1]))+
  ggtitle(substitute(paste("Gamma, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[1],s=bpar[1])))+
  ylab("f(x)")+
  annotate("segment", x = c(gammaE[1]), xend = c(gammaE[1]), 
           y = c(0), yend = c(0.04), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = gammaE[1], y = 0.06, 
           label = paste0("E(X)= ",gammaE[1]),size=4.5)+
  theme_bw()

gmaxlim2<-c(0,70)
pgmaE2<-ggplot(data = data.frame(x =gmaxlim2 ), aes(x)) +
  stat_function(fun = dgamma, n = 101, args = list(shape = apar[2], scale = bpar[2]))+
  ggtitle(substitute(paste("Gamma, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[2],s=bpar[2])))+
  ylab("f(x)")+
  annotate("segment", x = c(gammaE[2]), xend = c(gammaE[2]), 
           y = c(0), yend = c(0.04), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = gammaE[2], y = 0.06, 
           label = paste0("E(X)= ",gammaE[2]),size=4.5)+
  theme_bw()

pgmaE3<-ggplot(data = data.frame(x = gmaxlim), aes(gmaxlim)) +
  stat_function(fun = dgamma, n = 101, args = list(shape = apar[3], scale = bpar[3]))+
  ggtitle(substitute(paste("Gamma, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[3],s=bpar[3])))+
  ylab("f(x)")+
  annotate("segment", x = c(gammaE[3]), xend = c(gammaE[3]), 
           y = c(0), yend = c(0.04), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = gammaE[3], y = 0.05, 
           label = paste0("E(X)= ",gammaE[3]),size=4.5)+
  theme_bw()

apar<-c(0.7,2.5,5)
bpar<-c(0.7,5,1)
betaE<-apar/(apar+bpar)
btaxlim<-c(0, 1)
pbtaE1<-ggplot(data = data.frame(x = btaxlim), aes(x)) +
  stat_function(fun = dbeta, n = 101, args = list(shape1 = apar[1], shape2 = bpar[1]))+
  ggtitle(substitute(paste("Beta, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[1],s=bpar[1])))+
  ylab("f(x)")+
  annotate("segment", x = c(betaE[1]), xend = c(betaE[1]), 
           y = c(0), yend = c(1.2), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = betaE[1], y = 1.3, 
           label = paste0("E(X)= ",betaE[1]),size=4.5)+
  theme_bw()

pbtaE2<-ggplot(data = data.frame(x = btaxlim), aes(x)) +
  stat_function(fun = dbeta, n = 101, args = list(shape1 = apar[2], shape2 = bpar[2]))+
  ggtitle(substitute(paste("Beta, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[2],s=bpar[2])))+
  ylab("f(x)")+
  annotate("segment", x = c(betaE[2]), xend = c(betaE[2]), 
           y = c(0), yend = c(1.2), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = betaE[2], y = 1.3, 
           label = paste0("E(X)= ",round(betaE[2],3)),size=4.5)+
  theme_bw()

pbtaE3<-ggplot(data = data.frame(x = btaxlim), aes(x)) +
  stat_function(fun = dbeta, n = 101, args = list(shape1 = apar[3], shape2 = bpar[3]))+
  ggtitle(substitute(paste("Beta, ", alpha," = ",v,", ",beta,"= ",s),list(v=apar[3],s=bpar[3])))+
  ylab("f(x)")+
  annotate("segment", x = c(betaE[3]), xend = c(betaE[3]), 
           y = c(0), yend = c(1.1), colour = "black", size=1, alpha=0.6, arrow=arrow(length = unit(0.07, "npc")))+
  annotate("text", x = betaE[3], y = 1.2, 
           label = paste0("E(X)= ",round(betaE[3],3)),size=4.5)+
  theme_bw()

