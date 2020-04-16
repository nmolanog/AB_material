p_load( samplingbook )

sample.size.prop(0.09, P = .155, N = Inf, level = 0.95)
sample.size.prop(0.05, P = .155, N = Inf, level = 0.95)

sample.size.prop(0.06, P = .07, N = Inf, level = 0.95)
sample.size.prop(0.05, P = .07, N = Inf, level = 0.95)

sample.size.prop(0.09, P = .1, N = Inf, level = 0.95)
sample.size.prop(0.05, P = .1, N = Inf, level = 0.95)

sample.size.prop(0.12, P = .6, N = Inf, level = 0.95)
sample.size.prop(0.05, P = .6, N = Inf, level = 0.95)

sample.size.prop(e, P = 0.073, N = Inf, level = 0.95)

#600000 en colombia

target_props<-seq(0.06,.94,by=.01)

ns<-rep(NA,length(target_props))
for(i in seq_along(target_props)){
  ns[i]<-sample.size.prop(.05, P = target_props[i], N = Inf, level = 0.95)$n
}

plot(target_props,ns,main = "clasic")

p_load( binomSamSize )

target_props<-seq(0.06,.94,by=.01)
ns<-rep(NA,length(target_props))
for(i in seq_along(target_props)){
  ns[i]<-ciss.midp(target_props[i], alpha=0.05,d=.05)
}

plot(target_props,ns,main = "ciss.mid")

###experiments on e
my_e<-seq(0.005,.2,by=.001)
ns<-rep(NA,length(my_e))
for(i in seq_along(my_e)){
  ns[i]<-sample.size.prop(my_e[i], P = .25, N = Inf, level = 0.95)$n
}

plot(my_e,ns,main = "clasic")
data.frame(my_e,ns)

###experiments on N
Ns<-c(seq(10,500,by=10),seq(1000,10000,by=1000),seq(10000,100000,by=10000))
ns<-rep(NA,length(Ns))
for(i in seq_along(Ns)){
  ns[i]<-sample.size.prop(.025, P = .25, N = Ns[i], level = 0.95)$n
}

plot(Ns,ns,main = "clasic")
data.frame(my_e,ns)
