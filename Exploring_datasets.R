rm(list=ls())
library(pacman)
library(tidyverse)
p_load(boot)
data("beaver")
summary(beaver)

plot(temp~time, data = beaver)
plot(temp~activ, data = beaver)###this one

p_load(MASS)
data("cats")
summary(cats)
###los 3 sirven
plot(Hwt~Bwt, data = cats)
plot(Hwt~Sex, data = cats)
plot(Bwt~Sex, data = cats)


data(cd4)
summary(cd4)
cd4%>%gather()->long_cd4
plot(value~factor(key),data=long_cd4)


data("urine")
summary(urine)
plot(gravity~factor(r),data=urine)
plot(osmo~cond ,data=urine)
plot(urine[,-1])###lots of posibilities


p_load(datasets)
data("DNase")
summary(DNase)
plot(density~conc,data=DNase)###good nonlinear example

data("Indometh")
summary(Indometh)
plot(conc~time,data=Indometh)###good nonlinear example

data("Puromycin")
summary(Puromycin)
plot(conc~rate,data=Puromycin)
plot(conc~state,data=Puromycin)
plot(rate~state,data=Puromycin)


data("ToothGrowth")###good one
summary(ToothGrowth)
plot(len~supp,data=ToothGrowth)
plot(len~factor(dose),data=ToothGrowth)


summary(trees)###good one for lm
plot(trees)

p_load("HSAUR")
data("plasma")
summary(plasma)
plot(fibrinogen~globulin,data=plasma)###no trend example.
plot(fibrinogen~ESR,data=plasma)
plot(globulin~ESR,data=plasma)
