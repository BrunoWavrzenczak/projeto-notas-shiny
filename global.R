library(shiny)
library(tidyverse)
library(shinydashboard)

tb <- read.csv('projeto notas - Página1.csv')

a <- function(x) {
    ifelse(x <= 4, '[0,4]', ifelse(x > 4 & x <= 7, '(4,7]', '(7,10]'))
}


tb <- transform(tb, intervalo = a(x = tb$notas))
