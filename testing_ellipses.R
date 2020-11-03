rm(list=ls())
options(max.print=999999)
library(pacman)
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

###data for vertical curve
my_dnorm<- function(x, mean = 0, sd = 1, log = FALSE, new_loc, multplr){
  new_loc+dnorm(x, mean , sd, log)*multplr
}

##margina Y distribution
yden<-data.frame(y=seq(48,82,length.out = 100),x=my_dnorm(seq(48,82,length.out = 100),my_mean[2],sd_vec[2],new_loc=0,multplr=100))

##conditional distribution
givenX<-34
mu_givenX<-my_int+givenX*my_slp
sigma2_givenX<-(1-mycors[i]^2)*V[2,2]
y_givenX_range<-seq(mu_givenX-3*sqrt(sigma2_givenX),mu_givenX+3*sqrt(sigma2_givenX),length.out = 100)

# yden_x<-data.frame(y=y_givenX_range,
#                    x=my_dnorm(y_givenX_range,mu_givenX,sqrt(sigma2_givenX),new_loc=givenX,multplr=80))

yden_x<-data.frame(y=y_givenX_range,
                   x=my_dnorm(y_givenX_range,mu_givenX,sqrt(sigma2_givenX),new_loc=0,multplr=80))


data.grid <- expand.grid(x = seq(my_mean[1]-3*sd_vec[1], my_mean[1]+3*sd_vec[1], length.out=200),
                         y = seq(my_mean[2]-3*sd_vec[2], my_mean[2]+3*sd_vec[2], length.out=200))
q.samp <- cbind(data.grid, prob = dmvnorm(data.grid, mean = my_mean, sigma = V))
ggplot(q.samp, aes(x=x, y=y, z=prob)) + 
  geom_contour() + theme_bw()+ 
  geom_abline(intercept = my_int, slope = my_slp, color="red", 
                                           linetype="dashed")+
  stat_function(fun = my_dnorm, n = 101, args = list(mean = my_mean[1], sd = sd_vec[1], new_loc=35,multplr=100),color=1) +
  geom_path(aes(x=x,y=y), data = yden,inherit.aes = FALSE) +
  geom_path(aes(x=x,y=y), data = yden_x,inherit.aes = FALSE,color=1,linetype="dashed") +
  
  geom_vline(xintercept = givenX,linetype="dashed")+ xlim(0, 50)+
  annotate("segment", x = c(givenX), xend = c(max(yden_x[,"x"])), 
           y = c(mu_givenX), yend = c(mu_givenX), colour = "red", size=.5, alpha=0.6, arrow=arrow(length = unit(0.03, "npc")))

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

z0_sim<-rmvnorm(200,my_mean,V) %>% data.frame()
colnames(z0_sim)<-c("x","y")
p3<-z0_sim %>% ggplot(aes(x=x,y=y))+geom_point()+theme_bw()
p2+geom_point(data=z0_sim,aes(x=x,y=y,z=0))



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
mycors<-seq(-.95,.95,length.out = 9)
sd_vec<-c(5,7)

pl_list<-list()
for(i in seq_along(mycors)){
  #i<-3
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
  
  # persp(my_x, my_y, my_z, theta = -25, phi = 45, expand = 0.5,xlab="x",ylab="y",zlab="f(x,y)",col = as.character(zzz))
  # p1 <- recordPlot()  
  
  data.grid <- expand.grid(x = seq(my_mean[1]-3*sd_vec[1], my_mean[1]+3*sd_vec[1], length.out=200),
                           y = seq(my_mean[2]-3*sd_vec[2], my_mean[2]+3*sd_vec[2], length.out=200))
  q.samp <- cbind(data.grid, prob = dmvnorm(data.grid, mean = my_mean, sigma = V))
  
  
  pl_list[[i]]<-ggplot(q.samp, aes(x, y, z = prob)) + 
    geom_contour(aes(color = ..level..), bins = 11, size = 1) + 
    scale_color_gradientn(colours = jet.colors(11)) +
    theme_bw()+ theme(legend.position = "none")+
    ggtitle(substitute(paste(rho,"= ", v),list(v=mycors[i])))
  
}
gridExtra::grid.arrange(grobs =pl_list, 
                        ncol = 3, nrow = 3)
