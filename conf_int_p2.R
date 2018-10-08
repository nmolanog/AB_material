my_mean<-485.67
my_var<-95.75
my_n<-55

my_confint<-function(my_mean,my_var,my_n,alpha_ic,alpha_ph,h0_val,rej){
  n_1<-my_n-1
  t_q<-qt(alpha_ic/2,n_1)
  s_n<-sqrt(my_var)/sqrt(my_n)
  nS2<-n_1*my_var
  mean_conf<-c(my_mean+t_q*s_n,my_mean-t_q*s_n)
  var_conf<-c(nS2/qchisq(1-alpha_ic/2,n_1),nS2/qchisq(alpha_ic/2,n_1))
  res1<-data.frame(Param=c("E","V"),rbind(mean_conf,var_conf))
  EPO<-n_1*my_var/h0_val
  if(rej == "neq"){
    q_reject<-qchisq(c(alpha_ph/2,1-alpha_ph/2),n_1)
    if(EPO>q_reject[1] & EPO<q_reject[2]){
      concl<-"accept h0"
      }else {concl<-"reject h0"}
  }else if(rej == "less"){
    q_reject<-qchisq(alpha_ph,n_1)
    if(EPO>q_reject){
      concl<-"accept h0"
    }else {concl<-"reject h0"}
  }else if(rej == "great"){
    q_reject<-qchisq(1-alpha_ph,n_1)
    if(EPO<q_reject){
      concl<-"accept h0"
    }else {concl<-"reject h0"}
  }
  return(list(conf=res1,EPO=EPO,q_reject=q_reject,concl=concl))
}
my_confint(485.67,95.75,55,.1,.05,93,"less")
my_confint(493.29,62.67,35,.1,.05,100,"less")
my_confint(505.34,135,45,.1,.05,98,"great")

