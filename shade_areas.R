rm(list=ls())
library(tidyverse)
ggplot(data = data.frame(x = c(-5, 5)), aes(x)) +
  stat_function(fun = dt, args = list(df = 70))+
  geom_segment(aes(x=qt(.95,70),xend=qt(.95,70),y=0,yend=dt(qt(.95,70),70)))+
  stat_function(fun = dt, args = list(df = 70),
                xlim = c(qt(.95,70),5),
                geom = "area",fill="red",alpha=0.5)+
  stat_function(fun = dt, args = list(df = 70),
                xlim = c(0,3),
                geom = "area",fill="blue",alpha=0.5)
  ylab("f(t)")+xlab("t")+
  theme_bw()
