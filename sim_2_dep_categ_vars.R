library(pacman)
p_load(tidyverse)
p_load(MCMCpack)

N<-1000
v1_nc<-4
v2_nc<-3

v1_props<-rdirichlet(1,rep(1,v1_nc))%>%drop
names(v1_props)<-letters[1:length(v1_props)]
v2_given_v1probs<-rdirichlet(v1_nc,rep(1,v2_nc))
colnames(v2_given_v1probs)<-LETTERS[1:v2_nc]
rownames(v2_given_v1probs)<-names(v1_props)

N*v1_props%>%round(1)->v1_n

z0<-data.frame(v1=rep(names(v1_props),v1_n),v2=NA)

for(i in names(v1_props)){
  z0[z0$v1 %in% i,"v2"]<-sample(colnames(v2_given_v1probs),v1_n[i],T,v2_given_v1probs[i,])
}
z0$v2<-factor(z0$v2)
summary(z0)
table(z0)
