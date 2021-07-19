rm(list=ls())
library(mvtnorm)
library(animation)
library(tidyverse)
library(GA)

my_mean<-c(25,65)
mycors<-seq(-1,1,by=.25)
sd_vec<-c(5,7)

i<-3
temp_cor<-matrix(c(1,mycors[i],
                   mycors[i],1),
                 byrow = T,ncol=2)
V<-sd_vec %*% t(sd_vec) *temp_cor

my_x<-seq(my_mean[1]-3*sd_vec[1], my_mean[1]+3*sd_vec[1], length.out=25)
my_y<-seq(my_mean[2]-3*sd_vec[2], my_mean[2]+3*sd_vec[2], length.out=25)
temp_f<-function(a,b){dmvnorm(cbind(a,b), my_mean,V)}
my_z<-outer(my_x, my_y,temp_f)
my_theta<-seq(-45,45,length.out = 40)
my_theta<-c(my_theta,rev(my_theta))

nlevels<-20
my_zlim <- range(my_z, finite = TRUE)
my_levels <- pretty(my_zlim, nlevels)
zz <- (my_z[-1, -1] + my_z[-1, -ncol(my_z)] + my_z[-nrow(my_z), -1] + my_z[-nrow(my_z), 
                                                         -ncol(my_z)])/4
cols <- jet.colors(length(my_levels) - 1)
zzz <- cut(zz, breaks = my_levels, labels = cols)


saveGIF({
  ani.options(interval = 0.2, nmax = length(my_theta))
  ## use a loop to create images one by one
  for (i in 1:ani.options('nmax')) {
    persp(my_x, my_y, my_z, theta = my_theta[i], phi = 45, expand = 0.5,xlab="x",ylab="y",zlab="f(x,y)",col = as.character(zzz))
    ani.pause()   ## pause for a while ('interval')
  }
}, interval = 0.05, movie.name = 'bivar_norm_surf.gif', ani.width = 600, ani.height = 500)

res<-list()
###export data
for(i in seq_along(my_x) ){
  for(j in seq_along(my_y)){
    res[[paste0(i,"_",j)]]<-c(my_x[i],my_y[j],my_z[i,j])
  }
}

res_df<-res %>% reduce(rbind)
colnames(res_df)<-c("x","y","z")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, sheetName = "data")
openxlsx::writeData(wb,x=res_df , sheet = "data")
openxlsx::saveWorkbook(wb,"/home/nicolas/Downloads/data_surf_v0.xlsx",TRUE)