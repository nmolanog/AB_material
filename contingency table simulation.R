rm(list=ls())
options(max.print=999999)
library(pacman)
p_load(here)
p_load(openxlsx)
p_load(tidyverse)
p_load(epitab)

ca_ctr_r<-.3

n<-250
nCA<-round(n*ca_ctr_r)
z0<-data.frame(status=c(rep("CA",nCA),rep("CTR",n-nCA)))
z0$exposition<-NA
exp_CA<-.45
exp_CTR<-.19

z0[z0$status %in% "CA","exposition"]<-ifelse(runif(nCA)<exp_CA,"yes","no")
z0[z0$status %in% "CTR","exposition"]<-ifelse(runif(n-nCA)<exp_CA,"yes","no")

z0$exposition<-factor(z0$exposition)

summary(z0)
res<-table(z0)
res0<-t(table(z0)%>%addmargins)
res<-kable()

contingency_table(list("status"="status"),outcomes = list("exposition"="exposition"),crosstab_funcs=list(freq()),data=z0)
