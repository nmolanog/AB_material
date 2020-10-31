###########################
###dpois
###########################
rm(list=ls())
library(animation)
library(tidyverse)
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

data.grid <- expand.grid(x = seq(my_mean[1]-3.5*sd_vec[1], my_mean[1]+3.5*sd_vec[1], length.out=200),
                         y = seq(my_mean[2]-3.5*sd_vec[2], my_mean[2]+3.5*sd_vec[2], length.out=200))
q.samp <- cbind(data.grid, prob = dmvnorm(data.grid, mean = my_mean, sigma = V))

givenX<-seq(my_mean[1]-2.7*sd_vec[1], my_mean[1]+2.7*sd_vec[1], length.out=40)
givenX<-c(givenX,rev(givenX))
saveGIF({
  ani.options(interval = 0.2, nmax = length(givenX))
  ## use a loop to create images one by one
  for (j in 1:ani.options('nmax')) {
    # j<-10
    mu_givenX<-my_int+givenX[j]*my_slp
    sigma2_givenX<-(1-mycors[i]^2)*V[2,2]
    y_givenX_range<-seq(mu_givenX-3*sqrt(sigma2_givenX),mu_givenX+3*sqrt(sigma2_givenX),length.out = 100)
    
    yden_x<-data.frame(y=y_givenX_range,
                       x=my_dnorm(y_givenX_range,mu_givenX,sqrt(sigma2_givenX),new_loc=0,multplr=85))
    p1<-
      ggplot(q.samp, aes(x=x, y=y, z=prob)) + 
      geom_contour() + theme_bw()+ 
      geom_abline(intercept = my_int, slope = my_slp, color="red", 
                  linetype="dashed")+
      stat_function(fun = my_dnorm, n = 101, args = list(mean = my_mean[1], sd = sd_vec[1], new_loc=35,multplr=100),color=1) +
      geom_path(aes(x=x,y=y), data = yden,inherit.aes = FALSE) +
      geom_path(aes(x=x,y=y), data = yden_x,inherit.aes = FALSE,color=1,linetype="dashed") +
      
      geom_vline(xintercept = givenX[j],linetype="dashed")+ 
      xlim(0, 50)+ylim(33.5, 90)+
      annotate("segment", x = c(givenX[j]), xend = c(max(yden_x[,"x"])), 
               y = c(mu_givenX), yend = c(mu_givenX), colour = "red", size=.5, alpha=0.6, arrow=arrow(length = unit(0.03, "npc")))
    
    print(p1)
    ani.pause()   ## pause for a while ('interval')
  }
}, interval = 0.05, movie.name = 'biv_norm.gif', ani.width = 480, ani.height = 400)
