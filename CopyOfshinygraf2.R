library(shiny)
library(tidyverse)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput('periodo','qual periodo?',c('1','2'),'1'),
      uiOutput('materias')
    ),
    mainPanel(
      plotOutput('p1'),
      plotOutput('p2')
    )
  )
)

server <- function(input,output){
  
  a <- function(x){
    ifelse(x<=4,sum(x<=4),ifelse(x>7,paste(sum(x>7),' '),paste(sum(x>4 & x<=7),'  ')))
  }
  
  
  p1calculo <- data.frame(notas = c(3.5,0.5,9.5,8.7,0,0.5,0,9.5,0.5,1,0,4.5,2,
                                    2.5,0.5,0,7.2,2,6,4,0,0.5,6,4.7,1,9,3,7.5,
                                    0,2,0.5,0,3.5,1,0.5,9,0.5,0.5))
  p1calculo <- transform(p1calculo, legenda = as.character(a(x = p1calculo$notas)))
                          
   
  matriz <- data.frame(notas = c(5.4,4.8,4.6,9.7,7.7,5.7,8.3,5.7,2.3,8,3.3,4.4,6.2,
                                 3.7,5.7,6.1,7.2,2.9,2.2,6.1,1.4,4.5,7.8,7.3,4.7,
                                 7.9,5.3,9.6,6.3,2.2,2.5,0.3,5,2.6,8.4,6.4,3.1))
  matriz <- transform(matriz, legenda = as.character(a(x = matriz$notas)))
                       
  basica <- data.frame(notas = c(6.1,6.9,5.6,9.6,8.6,7.5,8.6,5.9,2.2,9.3,0.1,3.1,2.7,6.9,5.5,7.3,6,
                                6.4,3.2,0.1,6.4,0.1,1.8,0.1,0.1,8.2,1.8,9.2,6.1,6.2,9.4,9.5,7.2,6.7,2.9,
                                1.1,6,5.5,5.7,9.7,7.2))
  basica <- transform(basica, legenda = as.character(a(x = basica$notas))) 
  
  p2calculo <- data.frame(notas = c(0.5,1,7.6,7.5,1.5,6.2,0.5,4.5,0,5.3,0,2.1,
                                    3.8,0,4,4,1,4.2,2.5,0.5,9.5,0.5,6.5,3.5,3.5,
                                    4,10,3,1))
  p2calculo <- transform(p2calculo, legenda = as.character(a(x = p2calculo$notas)))
                         
  
  output$materias <- renderUI(
    switch(input$periodo,
           '1' = selectInput('mat1','materia',
                             c('estatistica basica','matrizes','funções'),
                             'estatistica basica'),
           '2' = selectInput('mat2','materia',c('calculo-1','GA'),'calculo-1'))
  )
    
  output$p1 <- renderPlot(
    if(input$periodo == '1'){
      switch(input$mat1,
             'estatistica basica' = ggplot(basica, aes(x = notas, fill = legenda))+
               geom_histogram(boundary = 0,binwidth = 1, color = 'black')+
               scale_x_continuous(limits = c(0,10), breaks = seq(0,10,1))+
               labs(title = 'media final de estatistica e inferencial basica', x = 'notas',
                    y = 'frequencia')+
               scale_fill_manual('legenda',values = c(2:4))+
               geom_vline(aes(xintercept = mean(notas), color = 'media'), size = 1.5)+
               scale_color_manual('',values = c('black')),
             
             'matrizes' = ggplot(matriz, aes(x = notas, fill = legenda))+
               geom_histogram(boundary = 0, binwidth = 1, color = 'black')+
               scale_x_continuous(limits = c(0,10), breaks = seq(0,10,1))+
               labs(title = 'media final de matrizes', x = 'notas', y = 'frequencia')+
               scale_fill_manual('legenda',values = c(2:4))+
               geom_vline(aes(xintercept = mean(notas), color = 'media'), size = 1.5)+
               scale_color_manual('',values = c('black')),
             
             'funções' = 'funções')
    } else{
      switch(input$mat2,
             'calculo-1' = ggplot(p1calculo, aes(x = notas, fill = legenda)) +
               geom_histogram(color = 'black',binwidth = 1, boundary=0) +
               scale_x_continuous(breaks = seq(0, 15, 1), limits = c(0,10)) +
               labs(x = 'notas', y = 'frequencia', title = 'p1 calculo-1') +
               scale_y_continuous(breaks = seq(0,20,2),limits = c(0,20)) +
               scale_fill_manual('legenda',values = c(2:4))+
               geom_vline(aes(xintercept = mean(notas), 
                              color = 'media'), size = 1.5)+
               scale_color_manual('',values = c(media = 'black')),
             
             'GA' = 'GA')
    }
  )
  
  output$p2 <- renderPlot(
    if(input$periodo == '2'){
      switch(input$mat2,
             'calculo-1' = ggplot(p2calculo, aes(x = notas, fill = legenda))+
               geom_histogram(color = 'black', binwidth = 1, boundary = 0)+
               scale_x_continuous(limits = c(0,10), breaks = seq(0,10,1))+
               labs(title = 'p2 calculo-1', x = 'notas', y = 'frequencia')+
               scale_fill_manual('legenda',values = c(2:4))+
               geom_vline(aes(xintercept = mean(notas), color = 'media'), size = 1.5)+
               scale_color_manual('',values = c('black')),
             'GA' = 'GA')
    }
  )  
}

shinyApp(ui,server)






























