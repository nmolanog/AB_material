rm(list=ls())
list.of.packages <- c("pacman")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(pacman)
p_load(tidyverse)
p_load(openxlsx)


alpha<-c(0.005,0.025,0.05,0.1)
GL1<-c(92,93,94,54,55,56)
res_t1<-outer(GL1,c(alpha,rev(1-alpha)),function(x,y){round(qt(y,x),4)})
colnames(res_t1)<-paste0("qT_",c(alpha,rev(1-alpha)))
res_t1<-cbind(GL1,res_t1)

res_chi1<-outer(GL1,c(alpha,rev(1-alpha)),function(x,y){round(qchisq(y,x),4)})
colnames(res_chi1)<-paste0("qChi_",c(alpha,rev(1-alpha)))
res_chi1<-cbind(GL1,res_chi1)


GL2<-c(34,35,36,99,100,101)
res_t2<-outer(GL2,c(alpha,rev(1-alpha)),function(x,y){round(qt(y,x),4)})
colnames(res_t2)<-paste0("qT_",c(alpha,rev(1-alpha)))
res_t2<-cbind(GL2,res_t2)

res_chi2<-outer(GL2,c(alpha,rev(1-alpha)),function(x,y){round(qchisq(y,x),4)})
colnames(res_chi2)<-paste0("qChi_",c(alpha,rev(1-alpha)))
res_chi2<-cbind(GL2,res_chi2)


GL3<-c(44,45,46,97,98,99)
res_t3<-outer(GL3,c(alpha,rev(1-alpha)),function(x,y){round(qt(y,x),4)})
colnames(res_t3)<-paste0("qT_",c(alpha,rev(1-alpha)))
res_t3<-cbind(GL3,res_t3)

res_chi3<-outer(GL3,c(alpha,rev(1-alpha)),function(x,y){round(qchisq(y,x),4)})
colnames(res_chi3)<-paste0("qChi_",c(alpha,rev(1-alpha)))
res_chi3<-cbind(GL3,res_chi3)


wb <- createWorkbook()
addWorksheet(wb, sheetName = "pa_t")
writeData(wb,x=res_t1, sheet = "pa_t")
addWorksheet(wb, sheetName = "pa_chi")
writeData(wb,x=res_chi1 , sheet = "pa_chi")

addWorksheet(wb, sheetName = "pb_t")
writeData(wb,x=res_t2, sheet = "pb_t")
addWorksheet(wb, sheetName = "pb_chi")
writeData(wb,x=res_chi2 , sheet = "pb_chi")

addWorksheet(wb, sheetName = "pc_t")
writeData(wb,x=res_t3, sheet = "pc_t")
addWorksheet(wb, sheetName = "pc_chi")
writeData(wb,x=res_chi3 , sheet = "pc_chi")

saveWorkbook(wb,paste("tables_p2",".xlsx",sep=""),TRUE)
