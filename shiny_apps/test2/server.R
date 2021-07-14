#######################
###load data
#######################
library(tidyverse)
library(shiny)

function(input, output, session){
  plt_params<-reactive({
    if(input$which_den == "Normal"){
      c(mu=input$mu,s2=input$sigma2,-150, 150)
    }else if(input$which_den == "Poisson"){
      c(mu=input$lambda)
    }
    
  })
  output$p1 <- renderPlot({
    if(input$which_den == "Normal"){
      ggplot(data = data.frame(weight = plt_params()[3:4]), aes(plt_params()[3:4])) +
        stat_function(fun = dnorm, n = 101, args = list(mean = plt_params()[1], sd =plt_params()[2] %>% sqrt),color=1) +
        ylab("f(x)") +xlab("x")+
        scale_x_continuous(breaks=seq(plt_params()[3],plt_params()[4], by=20)) +
        annotate("segment", x = plt_params()[1], xend = plt_params()[1],
                 y = 0, yend = dnorm(plt_params()[1],plt_params()[1],sqrt(plt_params()[2])), colour = "#CC3366", size=1, alpha=1)+
        ggtitle(substitute(paste("Normal density function: ", mu," = ",v,", ",sigma^2,"= ",s),
                           list(v=plt_params()[1],s=plt_params()[2])))+
        theme_bw()
    }else if(input$which_den == "Poisson"){
      dtf_pois<-data.frame(x=(0:125),y=dpois(0:125,plt_params()[1]))
      dtf_pois%>%ggplot(aes(x=x,y=y))+geom_point()+
        ylab("f(x)")+ theme_bw()+ 
        coord_cartesian(xlim = c(0, 125), ylim = c(0, .4))+
        ggtitle(substitute(paste("Poisson density function: ", lambda," = ",v),
                           list(v=plt_params()[1])))
    }
  })
}
