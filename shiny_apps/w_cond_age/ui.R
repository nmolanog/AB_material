library(pacman)
library(tidyverse)
library(shiny)
library(mvtnorm)


fluidPage(
  titlePanel("Distribution of weight conditional to age"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("cor",withMathJax("$$Select\\;\\rho$$"),-0.99,0.99,0.7),
      sliderInput("age","Select Age",10,50,25)
    ),
    mainPanel(
      plotOutput("p1"),
      tableOutput("t1")
    )
  )
)
