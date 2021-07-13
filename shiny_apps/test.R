#######################
###load data
#######################
rm(list=ls())
options(max.print=999999)
library(pacman)
p_load(here)
p_load(tidyverse)
p_load(shiny)
p_load(plotly)

#######################
###load data
#######################

ui <- fluidPage(
  titlePanel("test"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu",withMathJax("$$Select\\;\\mu$$"),-50,50,0,
                  animate = animationOptions(interval = 300, loop = TRUE)),
      sliderInput("sigma2",withMathJax("$$Select\\;\\sigma^2$$"),0,5000,25,step =50,
                  animate = animationOptions(interval = 300, loop = TRUE))
      ),
    mainPanel(
      plotOutput("p1"))
    )
)

server <- function(input, output, session){
  plt_params<-reactive({
    c(mu=input$mu,s2=input$sigma2,-150, 150)
  })
  output$p1 <- renderPlot({
    ggplot(data = data.frame(weight = plt_params()[3:4]), aes(plt_params()[3:4])) +
      stat_function(fun = dnorm, n = 101, args = list(mean = plt_params()[1], sd =plt_params()[2] %>% sqrt),color=1) +
      ylab("f(x)") +xlab("x")+
      scale_x_continuous(breaks=seq(plt_params()[3],plt_params()[4], by=20)) +
      annotate("segment", x = plt_params()[1], xend = plt_params()[1],
               y = 0, yend = dnorm(plt_params()[1],plt_params()[1],sqrt(plt_params()[2])), colour = "#CC3366", size=1, alpha=1)+
      ggtitle(substitute(paste("Normal density function: ", mu," = ",v,", ",sigma^2,"= ",s),
                         list(v=plt_params()[1],s=plt_params()[2])))+
      theme_bw()
  })
}
shinyApp(ui = ui, server = server)


