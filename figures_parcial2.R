rm(list=ls())
list.of.packages <- c("pacman")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(pacman)
p_load(tidyverse)

set.seed(500)

pdf(file=paste("cuantiles",".pdf",sep=""),width=6,height=4)
some_q<-sort(runif(3,0,1))
some_q<-c(0.7250118, 0.8336000, 0.9753142)
gl<-4
probs<-pchisq(qchisq(some_q[1],gl),gl)
for(i in seq_along(some_q)[-1]){
  probs<-c(probs,pchisq(qchisq(some_q[c(i-1,i)],gl),gl)%>%{.[2]-.[1]})
}
probs<-c(probs,pchisq(c(qchisq(some_q[3],gl),20),gl)%>%{.[2]-.[1]})
probs<-round(probs,3)

ggplot(data = data.frame(x =seq(0,20,length.out = length(some_q))), aes(x)) + ylab("f(x)")+xlab("x")+
  stat_function(fun = dchisq, args = list(df = gl))+
  geom_segment(aes(x=qchisq(some_q,gl),xend=qchisq(some_q,gl),y=rep(0,length(some_q)),yend=dchisq(qchisq(some_q,gl),gl)))+
  annotate("segment", x = c(qchisq(some_q[1],gl)/2,sum(qchisq(some_q[1:2],gl))/2,sum(qchisq(some_q[2:3],gl))/2,sum(c(qchisq(some_q[3],gl),20))/2), 
           xend = c(5,sum(qchisq(some_q[1:2],gl))/2,sum(qchisq(some_q[2:3],gl))/2,sum(c(qchisq(some_q[3],gl),20))/2), 
           y = c(0.15,.055,.015,0), yend = c(0.15,0.11,0.075,0.035), size=.6, alpha=0.6, arrow=arrow(angle=15))+
  annotate("text", x = c(5.8,sum(qchisq(some_q[1:2],gl))/2,sum(qchisq(some_q[2:3],gl))/2,sum(c(qchisq(some_q[3],gl),20))/2),
           y = c(0.15,c(0.11,0.075,0.035)+.005), 
           label = c(probs[1:2],"?",probs[4]), size=4 , fontface="bold")+
  scale_x_continuous("x", c(0,round(qchisq(some_q,gl),2),20), limits=c(0,20))+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

some_q<-sort(runif(3,0,1))
some_q<-c(0.2056958, 0.4676038, 0.8122781)
gl<-4
probs<-pchisq(qchisq(some_q[1],gl),gl)
for(i in seq_along(some_q)[-1]){
  probs<-c(probs,pchisq(qchisq(some_q[c(i-1,i)],gl),gl)%>%{.[2]-.[1]})
}
probs<-c(probs,pchisq(c(qchisq(some_q[3],gl),20),gl)%>%{.[2]-.[1]})
probs<-round(probs,3)

ggplot(data = data.frame(x =seq(0,20,length.out = length(some_q))), aes(x)) + ylab("f(x)")+xlab("x")+
  stat_function(fun = dchisq, args = list(df = gl))+
  geom_segment(aes(x=qchisq(some_q,gl),xend=qchisq(some_q,gl),y=rep(0,length(some_q)),yend=dchisq(qchisq(some_q,gl),gl)))+
  annotate("segment", x = c(1.4,sum(qchisq(some_q[1:2],gl))/2,sum(qchisq(some_q[2:3],gl))/2,10), 
           xend = c(.6,5,6.5,sum(c(qchisq(some_q[3],gl),20))/2), 
           y = c(0.14,.16,.08,0.005), yend = c(0.17,0.18,0.13,0.035), size=.6, alpha=0.6, arrow=arrow(angle=15))+
  annotate("text", x = c(.6,5,6.5,sum(c(qchisq(some_q[3],gl),20))/2),
           y = c(0.17,0.18,0.13,0.035)+.005, 
           label = c(probs[1:2],"?",probs[4]), size=4 , fontface="bold")+
  scale_x_continuous("x", c(0,round(qchisq(some_q,gl),2),20), limits=c(0,20))+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#some_q<-sort(runif(3,0,1))
some_q<-c(0.5121819,0.8288314,0.9254660)
gl<-4
probs<-pchisq(qchisq(some_q[1],gl),gl)
for(i in seq_along(some_q)[-1]){
  probs<-c(probs,pchisq(qchisq(some_q[c(i-1,i)],gl),gl)%>%{.[2]-.[1]})
}
probs<-c(probs,pchisq(c(qchisq(some_q[3],gl),20),gl)%>%{.[2]-.[1]})
probs<-round(probs,3)

ggplot(data = data.frame(x =seq(0,20,length.out = length(some_q))), aes(x)) + ylab("f(x)")+xlab("x")+
  stat_function(fun = dchisq, args = list(df = gl))+
  geom_segment(aes(x=qchisq(some_q,gl),xend=qchisq(some_q,gl),y=rep(0,length(some_q)),yend=dchisq(qchisq(some_q,gl),gl)))+
  annotate("segment", x = c(qchisq(some_q[1],gl)/2,sum(qchisq(some_q[1:2],gl))/2,sum(qchisq(some_q[2:3],gl))/2,11), 
           xend = c(4,sum(qchisq(some_q[1:2],gl))/2,sum(qchisq(some_q[2:3],gl))/2,sum(c(qchisq(some_q[3],gl),20))/2), 
           y = c(0.15,.075,.035,0.005), yend = c(0.175,0.13,0.075,0.035), size=.6, alpha=0.6, arrow=arrow(angle=15))+
  annotate("text", x = c(4,sum(qchisq(some_q[1:2],gl))/2,sum(qchisq(some_q[2:3],gl))/2,sum(c(qchisq(some_q[3],gl),20))/2),
           y = c(0.175,0.13,0.075,0.035)+.005, 
           label = c(probs[1:2],"?",probs[4]), size=4 , fontface="bold")+
  scale_x_continuous("x", c(0,round(qchisq(some_q,gl),2),20), limits=c(0,20))+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dev.off() 
