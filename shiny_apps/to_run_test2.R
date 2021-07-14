rm(list=ls())
options(max.print=999999)
library(pacman)
p_load(here)
p_load(tidyverse)
p_load(shiny)
getwd()
##test locally
runApp("test2")
#deploy in shiny
rsconnect::setAccountInfo(name='nicolas-molano',
                          token='xxxx`',
                          secret='<SECRET>')

rsconnect::deployApp("test2")
