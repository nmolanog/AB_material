library(pacman)
library(tidyverse)
library(shiny)
library(mvtnorm)

function(input, output, session){
  plt_params<-reactive({
    c(mycors=input$cor,givenX=input$age)
  })
  plt_params2<-reactive({
    my_mean<-c(25,65)
    mycors<-plt_params()["mycors"]
    sd_vec<-c(5,7)
    
    temp_cor<-matrix(c(1,mycors,
                       mycors,1),
                     byrow = T,ncol=2)
    V<-sd_vec %*% t(sd_vec) *temp_cor
    
    my_int<-(my_mean[2]-(V[1,2]*my_mean[1]/V[1,1]))
    my_slp<-V[1,2]/V[1,1]
    givenX<-plt_params()["givenX"]
    mu_givenX<-my_int+givenX*my_slp
    
    mu_givenX
  })
  output$p1 <- renderPlot({
    my_mean<-c(25,65)
    mycors<-plt_params()["mycors"]
    sd_vec<-c(5,7)
    
    temp_cor<-matrix(c(1,mycors,
                       mycors,1),
                     byrow = T,ncol=2)
    V<-sd_vec %*% t(sd_vec) *temp_cor
    
    my_int<-(my_mean[2]-(V[1,2]*my_mean[1]/V[1,1]))
    my_slp<-V[1,2]/V[1,1]
    
    ###data for vertical curve
    my_dnorm<- function(x, mean = 0, sd = 1, log = FALSE, new_loc, multplr){
      new_loc+dnorm(x, mean , sd, log)*multplr
    }
    
    ##conditional distribution
    givenX<-plt_params()["givenX"]
    mu_givenX<-my_int+givenX*my_slp
    sigma2_givenX<-(1-mycors^2)*V[2,2]
    y_givenX_range<-seq(mu_givenX-3*sqrt(sigma2_givenX),mu_givenX+3*sqrt(sigma2_givenX),length.out = 100)
    
    # yden_x<-data.frame(y=y_givenX_range,
    #                    x=my_dnorm(y_givenX_range,mu_givenX,sqrt(sigma2_givenX),new_loc=givenX,multplr=80))
    
    yden_x<-data.frame(y=y_givenX_range,
                       x=my_dnorm(y_givenX_range,mu_givenX,sqrt(sigma2_givenX),new_loc=0,multplr=80))
    
    
    data.grid <- expand.grid(x = seq(my_mean[1]-3*sd_vec[1], my_mean[1]+3*sd_vec[1], length.out=200),
                             y = seq(my_mean[2]-3*sd_vec[2], my_mean[2]+3*sd_vec[2], length.out=200))
    q.samp <- cbind(data.grid, prob = dmvnorm(data.grid, mean = my_mean, sigma = V))
    
    pcond1<-ggplot(q.samp, aes(x=x, y=y, z=prob)) + 
      geom_contour() + theme_bw()+ 
      xlim(0, 50)+
      ylim(40, 90)+xlab("age")+ylab("weight")
    
    pcond2<-pcond1+geom_path(aes(x=x,y=y), data = yden_x,inherit.aes = FALSE,color=1,linetype="dashed") +
      geom_vline(xintercept = givenX,linetype="dashed")
    
    pcond2
  })
  output$t1<-renderTable({
    data.frame(Parameter=c("Age","E(Weight|Age)"),Value=c(plt_params()["givenX"], plt_params2()[]))
  })
}