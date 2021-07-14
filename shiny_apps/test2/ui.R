#######################
###load data
#######################
library(tidyverse)
library(shiny)


fluidPage(
  titlePanel("test"),
  sidebarLayout(
    sidebarPanel(
      selectInput("which_den", "Select density function",
                  c("Normal", "Poisson")
      ),
      conditionalPanel(
        condition = "input.which_den == 'Normal'",
        sliderInput("mu",withMathJax("$$Select\\;\\mu$$"),-50,50,0,
                    animate = animationOptions(interval = 300, loop = TRUE)),
        sliderInput("sigma2",withMathJax("$$Select\\;\\sigma^2$$"),0,5000,25,step =50,
                    animate = animationOptions(interval = 300, loop = TRUE))
      ),
      conditionalPanel(
        condition = "input.which_den == 'Poisson'",
        sliderInput("lambda",withMathJax("$$Select\\;\\lambda$$"),0,100,10,
                    animate = animationOptions(interval = 300, loop = TRUE))
      )
    ),
    mainPanel(
      plotOutput("p1"))
  )
)
