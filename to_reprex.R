###############
rm(list=ls())
options(max.print=999999)
library(pacman)
p_load(tidyverse)
p_load(mvtnorm)
p_load(cowplot)
p_load(gridGraphics)
p_load(GA)


my_mean<-c(25,65)
mycors<-seq(-1,1,by=.25)
sd_vec<-c(5,7)

i<-3
temp_cor<-matrix(c(1,mycors[i],
                   mycors[i],1),
                 byrow = T,ncol=2)
V<-sd_vec %*% t(sd_vec) *temp_cor


my_x<-seq(my_mean[1]-3*sd_vec[1], my_mean[1]+3*sd_vec[1], length.out=20)
my_y<-seq(my_mean[2]-3*sd_vec[2], my_mean[2]+3*sd_vec[2], length.out=20)
temp_f<-function(a,b){dmvnorm(cbind(a,b), my_mean,V)}
my_z<-outer(my_x, my_y,temp_f)
nlevels<-20
my_zlim <- range(my_z, finite = TRUE)
my_levels <- pretty(my_zlim, nlevels)
zz <- (my_z[-1, -1] + my_z[-1, -ncol(my_z)] + my_z[-nrow(my_z), -1] + my_z[-nrow(my_z), 
                                                                           -ncol(my_z)])/4
cols <- jet.colors(length(my_levels) - 1)
zzz <- cut(zz, breaks = my_levels, labels = cols)


persp(my_x, my_y, my_z, theta = -25, phi = 45, expand = 0.5,xlab="x",ylab="y",zlab="f(x,y)",col = as.character(zzz))
p1 <- recordPlot()  

data.grid <- expand.grid(x = seq(my_mean[1]-3*sd_vec[1], my_mean[1]+3*sd_vec[1], length.out=200),
                         y = seq(my_mean[2]-3*sd_vec[2], my_mean[2]+3*sd_vec[2], length.out=200))
q.samp <- cbind(data.grid, prob = dmvnorm(data.grid, mean = my_mean, sigma = V))


p2<-ggplot(q.samp, aes(x, y, z = prob)) + 
  geom_contour(aes(color = ..level..), bins = 11, size = 1) + 
  scale_color_gradientn(colours = jet.colors(11)) +
  theme_bw()
plot_grid(p1, p2)
