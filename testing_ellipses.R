rm(list=ls())
options(max.print=999999)
library(pacman)
p_load(here)
p_load(openxlsx)
p_load(tidyverse)
p_load(mvtnorm)


my_mean<-c(25,65)
mycors<-seq(-1,1,by=.25)
sd_vec<-c(5,7)

i<-3
temp_cor<-matrix(c(1,mycors[i],
                   mycors[i],1),
                 byrow = T,ncol=2)
V<-sd_vec %*% t(sd_vec) *temp_cor

my_int<-(my_mean[2]-(V[1,2]*my_mean[1]/V[1,1]))
my_slp<-V[1,2]/V[1,1]

data.grid <- expand.grid(s.1 = seq(my_mean[1]-3*sd_vec[1], my_mean[1]+3*sd_vec[1], length.out=200),
                         s.2 = seq(my_mean[2]-3*sd_vec[2], my_mean[2]+3*sd_vec[2], length.out=200))
q.samp <- cbind(data.grid, prob = dmvnorm(data.grid, mean = my_mean, sigma = V))
ggplot(q.samp, aes(x=s.1, y=s.2, z=prob)) + 
  geom_contour() + theme_bw()+ 
  geom_abline(intercept = my_int, slope = my_slp, color="red", 
                                           linetype="dashed")


my_x<-seq(my_mean[1]-3*sd_vec[1], my_mean[1]+3*sd_vec[1], length.out=40)
my_y<-seq(my_mean[2]-3*sd_vec[2], my_mean[2]+3*sd_vec[2], length.out=40)
temp_f<-function(a,b){dmvnorm(cbind(a,b), my_mean,V)}
my_z<-outer(my_x, my_y,temp_f)
persp(my_x, my_y, my_z, theta = -10, phi = 45, expand = 0.5, col = "lightblue")

